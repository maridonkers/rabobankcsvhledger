(ns rabobankcsv
  "Converts Rabobank 2018 CSV-file format (as exported by Rabobank internet banking)  HLedger format.

  https://www.rabobank.nl/images/formaatbeschrijving-csv-extensie_29933458.pdf
  https://www.rabobank.nl/images/transactiesoortcodes_29842987.pdf

  Version 0.1.0

  See deps.edn and rabobankcsv bash script.

  DISCLAIMER: THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
  OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
  OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
  DAMAGE.

  Twitter: @maridonkers | Google+: +MariDonkers | GitHub: maridonkers"

  (:require [clojure.string :as string]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io]))

;; --- CONSTANTS

(def EXTENSION_HLEDGER ".journal")
(def SEPARATOR_NEWLINE " => ")
(def SEPARATOR_PAYEE " | ")
(def POSTFIX_ACCOUNT "  ")
(def POSTFIX_POSTING "  ")
(def PREFIX_POSTING "  ")

;; --- SUPPORTING FUNCTIONS

(defn extname
  "Gets file extension for path."
  [path]
  (let [ext (second (re-find #"^.*(\.[^.]+)$" path))]
    (if (empty? ext) "" ext)))

(defn basename
  "Gets basename for path with specified file extension."
  [path ext]
  (let [base (if (empty? ext)
               path
               (second (re-find (re-pattern (str "^(.*)(?:" ext ")$")) path)))]
    base))

(defn exists
  "Check if a path exists."
  [path]
  (let [f (io/file path)]
    (.exists f)))

(defn date-rabobank->hledger
  "Converts date from RABOBANK's yyyy-mm-dd to HLedger's yyyy/mm/dd."
  [s]
  (when (not (string/blank? s))
    (string/replace s #"(....)-(..)-(..)" "$1/$2/$3")))

(defn replace-newlines
  "Replaces newlines."
  [s]
  (when (not (string/blank? s))
    (string/replace s #"(\n)" SEPARATOR_NEWLINE)))

(defn replace-special-characters
  "Replaces the special characters with spaces."
  [s]
  (when (not (string/blank? s))
    (string/replace s #"[:;|\[\]]" " ")))

(defn trim+condense-whitespaces
  "Trims string and condenses several whitespaces to one space."
  [s]
  (when (not (string/blank? s))
    (-> s
        string/trim
        (string/replace #"\s+" " "))))

(defn somewhat-format->hledger
  "Somewhat escape and format string for hledger."
  [unformatted]
  (if (string/blank? unformatted)
    unformatted
    (let [formatted (-> unformatted
                        replace-newlines
                        replace-special-characters
                        trim+condense-whitespaces)]
      formatted)))

(defn format->hledger
  "Escape and format string for hledger."
  [unformatted]
  (if (string/blank? unformatted)
    unformatted
    (let [formatted (-> unformatted
                        replace-newlines
                        string/lower-case
                        replace-special-characters
                        trim+condense-whitespaces)]
      formatted)))

;; -------------
;; RABOBANK SEPA

(def DATE-REGEXP #"[0-9]{4}-[0-9]{2}-[0-9]{2}")
(def BBAN-REGEXP #"(?i)P?[0-9]+")
(def IBAN-REGEXP #"(?i)[A-Z]{2}[0-9]{2}[A-Z0-9]{4,}")
(def AMOUNT-REGEXP #"[+-][0-9]+,?[0-9]*")

(s/def ::iban-bban (s/and string?
                          #(<= (count %) 34)
                          (s/or :empty #(= (count %) 0)
                                :bban #(re-matches BBAN-REGEXP %)
                                :iban #(re-matches IBAN-REGEXP %))))

(s/def ::muntsoort (s/and string? #(<= (count %) 4)))

(s/def ::bic (s/and string? #(<= (count %) 11)))

(s/def ::volgnr (s/and string? #(<= (count %) 18)))

(s/def ::datum (s/and string?
                      #(<= (count %) 10)
                      (s/or :empty #(= (count %) 0)
                            :date #(re-matches DATE-REGEXP %))))

(s/def ::rentedatum ::datum)

(s/def ::bedrag (s/and string? #(<= (count %) 18) #(re-matches AMOUNT-REGEXP %)))

(s/def ::saldo ::bedrag)

(s/def ::naam (s/and string? #(<= (count %) 70)))

(s/def ::bic-tegenpartij (s/and string? #(<= (count %) 15)))

(s/def ::code (s/and string? #(<= (count %) 4)))

(s/def ::batch-id (s/and string? #(<= (count %) 35)))

(s/def ::transactiereferentie (s/and string? #(<= (count %) 35)))

(s/def ::machtigingskenmerk (s/and string? #(<= (count %) 35)))

(s/def ::incassant-id (s/and string? #(<= (count %) 35)))

(s/def ::betalingskenmerk (s/and string? #(<= (count %) 35)))

(s/def ::omschrijving (s/and string? #(<= (count %) 140)))

(s/def ::reden-retour (s/and string? #(<= (count %) 75)))

(s/def ::oorspr-bedrag (s/and string? #(<= (count %) 18)))

(s/def ::oorspr-munt (s/and string? #(<= (count %) 11)))

(s/def ::koers (s/and string? #(<= (count %) 11)))

;; "IBAN/BBAN","Munt","BIC","Volgnr","Datum","Rentedatum","Bedrag","Saldo na trn","Tegenrekening IBAN/BBAN","Naam tegenpartij","Naam uiteindelijke partij","Naam initiërende partij","BIC tegenpartij","Code","Batch ID","Transactiereferentie","Machtigingskenmerk","Incassant ID","Betalingskenmerk","Omschrijving-1","Omschrijving-2","Omschrijving-3","Reden retour","Oorspr bedrag","Oorspr munt","Koers"

;; Veld	Omschrijving			Type		Lengte	Inhoud/Toelichting
(s/def ::sepa-columns (s/cat
                       ;; 1	IBAN_BBAN	                Alfanumeriek	34	Eigen Rekeningnummer
                       :1 ::iban-bban
                       ;; 2	MUNTSOORT			Alfanumeriek	4	Muntsoort eigen rekening
                       :2 ::muntsoort
                       ;; 3	BIC			        Alfanumeriek	11	BIC eigen rekening
                       :3 ::bic
                       ;; 4	volgnr			        Alfanumeriek	18	Volgnummer
                       :4 ::volgnr
                       ;; 5	DATUM		  	        Datum           10	Verwerkings- of boekdatum. Formaat: EEJJ-MM-DD; Bijvoorbeeld 2017-07-31
                       :5 ::datum
                       ;; 6	RENTEDATUM			Datum	        10	(Rente|Valuta)datum. Formaat: EEJJ-MM-DD; Bijvoorbeeld 2017-07-31
                       :6 ::rentedatum
                       ;; 7	BEDRAG				Numeriek	18	Prefix +/-; ISO4217 decimalen. Decimalen worden weergegeven met een comma
                       :7 ::bedrag
                       ;; 8	SALDO_NA_TRANSACTIE		Numeriek	18	Prefix +/-; ISO4217 decimalen. Decimalen worden weergegeven met een comma
                       :8 ::saldo
                       ;; 9	TEGENREKENING_IBAN_BBAN		Alfanumeriek	34	Tegenrekeningnummer
                       :9 ::iban-bban
                       ;; 10	NAAM_TEGENPARTIJ		Alfanumeriek	70	Naam van de tegenpartij
                       :10 ::naam
                       ;; 11	NAAM_UITEINDELIJKE_PARTIJ	Alfanumeriek	70	Naam van de uiteindelijke partij
                       :11 ::naam
                       ;; 12	NAAM_INITIERENDE_PARTIJ	        Alfanumeriek	70	Naam van de initiërende partij
                       :12 ::naam
                       ;; 13	BIC_TEGENPARTIJ			Alfanumeriek	15	BIC rekening tegenpartij
                       :13 ::bic-tegenpartij
                       ;; 14	CODE			        Alfanumeriek	4	Transactiecode
                       :14 ::code
                       ;; 15	BATCH_ID			Alfanumeriek	35
                       :15 ::batch-id
                       ;; 16	TRANSACTIEREFERENTIE		Alfanumeriek	35
                       :16 ::transactiereferentie
                       ;; 17	MACHTIGINGSKENMERK		Alfanumeriek	35
                       :17 ::machtigingskenmerk
                       ;; 18	INCASSANT_ID		        Alfanumeriek	35
                       :18 ::incassant-id
                       ;; 19	BETALINGSKENMERK		Alfanumeriek	35
                       :19 ::betalingskenmerk
                       ;; 20	OMSCHRIJVING-1		        Alfanumeriek	140
                       :20 ::omschrijving
                       ;; 21	OMSCHRIJVING-2			Alfanumeriek	140
                       :21 ::omschrijving
                       ;; 22	OMSCHRIJVING-3			Alfanumeriek	140
                       :22 ::omschrijving
                       ;; 23	REDEN_RETOUR		        Alfanumeriek	75
                       :23 ::reden-retour
                       ;; 24	OORSPR_BEDRAG		        Alfanumeriek	18
                       :24 ::oorspr-bedrag
                       ;; 25	OORSPR_MUNT		        Alfanumeriek	11
                       :25 ::oorspr-munt
                       ;; 26	KOERS		                Alfanumeriek	11
                       :26 ::koers
                       ))

;; ---------
;; KYMYMONEY
;;
;; Beschrijving uitvoerformaat (which is a KMyMoney compatible import formaat).
;;
;; Veld	Omschrijving			Type
;; 1	NUMBER				Alfanumeriek
;; 2	DATE				Numeriek
;; 3	DEBIT/CREDIT			Numeriek
;; 4	CATEGORY (code)			Alfanumeriek
;; 5	PAYEE (tegenrekening)		Alfanumeriek
;; 6	MEMO (concatenated fields)	Alfanumeriek
;;
;; The MEMO field in the output is a concatenation of various fields
;; taken from the SEPA input. Spaces are inserted where required, (to
;; accommodate readability.)
;;

;; Set with output filenames, which is used to delete existing files
;; only once.
(def output-fnames (atom #{}))

(defn get-csv-columns
  "Gets CSV columns as vector. The enclosing quotes are
  removed. Nested quotes are not allowed in Rabobank SEPA."
  [csv-line]
  (let [columns (map second (re-seq #"\"([^\"]*)\"" csv-line))]
    
    (when-not (s/valid? ::sepa-columns columns)
      (println (str "\t" (s/explain ::sepa-columns columns))))
    columns))

(defn convert-description
  "Converts description."
  [cvs]

  (let [[_ _ _ _ _ _ _ _
         tegenrekening
         _ _ _ _ _ _ _
         machtigings-kenmerk incassant-id
         betalingskenmerk
         omschrijving-1 omschrijving-2 omschrijving-3
         _ _ _ _] cvs

        omschrijving (->>
                      (str omschrijving-1 omschrijving-2 omschrijving-3)
                      string/trim)

        extra (->> [machtigings-kenmerk incassant-id]
                   (map string/trim)
                   (interpose " ")
                   (filter #(seq %))
                   (apply str)
                   string/trim)]

    (str (when (seq tegenrekening) (str "[" tegenrekening "] "))
         (str (when (seq betalingskenmerk) (str betalingskenmerk " "))
              omschrijving
              (when (seq extra) (str " " extra))))))

(defn convert-columns
  "Converts columns in input CSV line to columns in output CSV line."
  [csv]

  (let [[_ _ _
         volgnr datum
         _
         bedrag
         _ _
         naam-tegenpartij
         _ _ _
         code
         _ _ _ _ _ _ _ _ _ _ _ _] csv

        bedrag (string/replace bedrag "," ".")
        negatief-bedrag (- 0 (Double. bedrag))
        description (convert-description csv)]

    (str (date-rabobank->hledger datum)
         " ! "
         (when (not (string/blank? volgnr)) (str "(" volgnr ")"))
         " " (somewhat-format->hledger naam-tegenpartij)
         (when (not (string/blank? naam-tegenpartij))" | ")
         (somewhat-format->hledger description)
         "\n"
         "  asset:rabobank:betaalrekening"
         "  " "EUR " bedrag
         "\n"
         "  equity:import:rabobank:"
         (format->hledger code)
         "  " "EUR " negatief-bedrag
         "\n\n")))

(defn convert-line
  "Converts CSV line. Appends to output file (determined by account
  number in first input file column: inputfilename#accountnumber.journal)."
  [fname csv-line]
  (let [csv (get-csv-columns csv-line)
        iban-bban (first csv)
        ext (extname fname)
        base (basename fname ext)
        ofname (str base "#" iban-bban ".journal")]

    (when (and (exists ofname)
               (not (contains? @output-fnames ofname)))
      (do (io/delete-file ofname)))

    ;; If the file already existed it was deleted and if didn't exist
    ;; it was also okay. So always add it to the set of output-fnames.
    (swap! output-fnames conj ofname)

    (spit ofname
          (convert-columns csv)
          :append true)

    iban-bban))

(defn convert
  "Converts CSV lines."
  [fname csv-str]
  (->> csv-str
       string/split-lines
       rest
       (map (partial convert-line fname))))

;; ----
;; MAIN

(defn -main [& args]
  (if-not (empty? args)
    (doseq [arg args]
      (println (str arg ":"))
      (let [lines (slurp arg :encoding "utf8")
            accounts (distinct (convert arg lines))]

        (println (str "\t"
                      (->> accounts
                           (interpose "\n\t")
                           (apply str)))))
      )
    (println (str "Usage: rabobankcsv pathname [pathname ...]\n\n"
                  "Converts Rabobank CSV export file format to HLedger."))))
