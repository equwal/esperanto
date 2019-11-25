;; Note: Only works if unicode is supported in symbols, due to espeanto's character
;; set.
(defpackage :esperanto
  (:use :cl :utils)
  (:nicknames :esp))

(in-package :esperanto)


(assert (unwind-protect (code-char  (1- 1114112)) nil))

(defun inside (from into)
  (if (eq into from)
      into
      (labels ((inside-aux (from into inclusive exclusive stop)
		 (if (= exclusive stop)
		     nil
		     (if (equalp (subseq from inclusive exclusive)
				 into)
			 into
			 (inside-aux from into (1+ inclusive) (1+ exclusive) stop)))))
	(inside-aux from into 0 (length into) (1+ (length from))))))

(defun concatenate-list (list)
  (reduce (lambda (x y) (concatenate 'string x y)) list))

(defvar *rules* nil
  "Assign each defrule here so they can be searched.")

(defvar *terminals* nil
  "Alist of terminals classes.")

(defun defterminals (class-name class-list)
  "Define a terminals class."
  (dolist (term class-list)
    (setf (symbol-plist term) (list 'terminal t)))
  (setf *terminals*
	(cons (list class-name class-list)
	      *terminals*)))

(defun terminal? (sym)
  "Is the symbol a terminal symbol?"
  (and (symbolp sym)
       (equal (symbol-plist sym) (list 'terminal t))))

(defun terminal-class? (class)
  "Determine if a class is a terminal class."
  (some #'(lambda (x)
	    (eql class (car x))) *terminals*))

(defun token? (class-item)
  (when (symbolp class-item)
    (member class-item (list '⚚ '☧ '⅏ 'NIL))))

(defun token-class? (list)
  "Determine if the class is tokenized (as opposed to being a terminal-class)."
  (and (listp list)
       (some #'token? list)))

(defun subterminal? (rule)
  (and (consp rule)
       (some #'(lambda (item)
		 (eq (car rule)
			  item)) (list '^ '+ 'g '*))
       (every #'terminal-class? (cdr rule))))

;; Notation:
;; Note that this is essentially a lispy BNF.
;; (+ ...) means 1 or more. NIL on failure.
;; (* ...) means 0 or more. Empty string on failure
;; (^ ...) means OR, so like a short circuit +. NIL on failure.
;; (g ...) is a grouping. Like a hybrid between logAND and concatenation.
;; There is an implicit (g ...) in defrule, so (defrule <rulename> (g ...))
;; defterminals are always of the form (defterminals <class> (<terminal> ...))

(defterminals 'correlative-prefix '(ki ti i ĉi neni))

(defterminals 'correlative-suffix '(a al am e el es o om u))

(defterminals 'prefix '(ali bo chef cxef cxi ĉef dis ek eks el elektro fi fuŝ ge i ki kun M mal mis ne neni nov  po pra pseŭdo re retro sen ti ĉi))

(defterminals 'noun-suffix '(ado ajho ajxo aĵo ano anto aro ato ĉjo eco edo ejo ero estro icho icxo iĉo ido iliardo ilio iliono ilo ingo ino into io ismo isto ito njo o oido ologo ono onto oto ulo ujo unto on))

(defterminals 'adjective-suffix '(a anta ata eca ega ema enda inda inta ita manka obla ona onta ota oza riĉa unta))

(defterminals 'verb-suffix '(adi i ighi igi igxi iĝi is os us ili as))

(defterminals 'adverb-suffix '(ante e eble ope))

(defterminals 'presuffix '(ant aĉ ad ar at ed eg estr et in ist unt ul er ing int ism ont ot um es it il iĉ icx ich an at ej))

(defterminals 'root '(absolut aĉ aĉet ad adres afer afrik ag aĝ aĵ ajn akcept
                      akv al ali almenaŭ alt am amas amerik amik ampleks amuz an
                      angl ankaŭ ankoraŭ anonc anstataŭ ant antaŭ aparat apart
                      aper ar aranĝ asoci aspekt at atend atent ating aŭ aŭd
                      aŭskult aŭtobus aŭt av banan batal baz bedaŭr bel bend
                      bezon bibliotek bild bilet bird bon botel bulgar cel cent
                      centr cert ceter cigared ĉambr ĉar ĉarm ĉe ĉef ĉeval ĉi
                      ĉiam ĉin ĉio ĉirkaŭ ĉiu ĉu da dan dank daŭr de debat decid
                      deĵor dek dekstr delegaci demand dev dezir diabl difin dir
                      dis diskriminaci diskut divers divid do dom don donac dorm
                      du dum ebl ec eĉ edz eg ej ek eks ekskurs ekster ekzempl
                      ekzempler ekzist el elekt en entrepren er erinac esenc
                      esper est estr et eŭrop eventual evolu facil fak fakt fal
                      famili far fart feliĉ ferm festival film fin finn firm fiŝ
                      flank flav flor flug foj for forges form fort fot franc
                      frank frat fraŭl fraz frenez fru funkci fuŝ ge german
                      giĉet grad grand gratul grav grup gvid ĝen ĝeneral ĝi ĝis
                      ĝoj ĝust ha hav hebre hejm help hieraŭ ho hodiaŭ hom hor
                      hotel ia iam ide ie iel ig iĝ il ili imag in ind infan
                      inform instru int inteligent inter interes interpret invit
                      io iom ir iran ist it ital iu ja jam japan jar je jen jes
                      jun ĵet kaj kamp kant kapabl kapt kar karot kart kaŝ kaz
                      ke kelk kia kial kiam kie kiel kilo kio kiom kiu klar
                      klopod knab kolor komenc komision komitat kompetent
                      komplet komplik kompren kon koncept koncern kongres
                      konkret konkur konsci konsent konserv konsil konsili
                      konsist konstant kontakt kontraŭ kontrol korb korespond
                      kost kovr kred kresk kri krom kruel kuir kuler kultur kun
                      kunikl kur kutim kvankam kvar kvazaŭ kvin la labor lag
                      lanĉ land las last laŭ lav leg lern lert leter lev li
                      liber libr lig lingv list lit liter literatur loĝ lok long
                      lu lud mal man manĝ manier mank map mar marŝ maŝin maten
                      material mem membr memor met mez mi miks mil minimum minut
                      mir moment mon monat mond mont montr morgaŭ mov mult naci
                      naĝ nask naŭ ne neces nederland neniam nenio neniu nepr
                      neŭtral ni nivel nokt nom normal nov nu nud numer nun nur
                      ofert oficial oft ok okaz okcident okup ol on oni ont
                      opini ord ordinar organiz orient ov pag paĝ paper pardon
                      parol part pas paŝ patr pend pens per perd perfekt period
                      persik person pet pied plaĉ plan plank plej plen pli plor
                      plu plur pokal pom pont popular por port post postul pov
                      prav precip preciz prefer preleg premi pren prepar preskaŭ
                      pret prez prezent prezid pri princip pro problem produkt
                      profesi profesor program proksim propon protest protokol
                      prov publik punkt pup pur rajt rakont rapid raport re
                      region regul reklam rekomend rekt relativ renkont respond
                      rest riĉ ricev rid rigard rilat rimark river romp rus saĝ
                      sal salon salt salut sam sat saŭn sci scienc se sed seg
                      seks sekv semajn sen senc send sent sep serĉ seri ses si
                      sid signif simil simpl simul sinjor siren sistem situaci
                      skandinavi skatol ski skrib soci sol solv sorĉ spec
                      special specif spert spinac star stat statut strat
                      struktur stult sub sud sufiĉ suk sukces super supoz supr
                      sur sved svis ŝaf ŝajn ŝanc ŝanĝ ŝat ŝi ŝip ŝir ŝlos ŝtel
                      tabl tabul tag tajp tamen task te teatr tekst teler tem
                      temp temperatur ten teren terur tia tial tiam tie tiel tim
                      tio tiom tiu tra traduk trajn trakt tranĉ trans tre tri
                      trink tro trov tuj tuk tuŝ tut uj ul um universal
                      universitat unu urb uson util uz valor varm vast ven vend
                      venk ver verk vesper vest veter vetur vi vid vin vir viv
                      vizit voĉ voj vojaĝ vol volv vort vulp zorg))

(defterminals 'oddball-suffixes '(aŭ j))

;; Horrible kludge: Probability of breakage: < 2/2^21
(defterminals 'tokens '(☧ nil ⅏ ⚚))

;; Compile-time functions for rules
(defun expand-terminal (class)
  "Get the expansion of a terminal class."
  (some #'(lambda (x)
	    (if (eql class (car x))
		(cadr x))) *terminals*))

(defun append-each (lists &optional acc)
  (if (null lists)
      acc
      (append-each (cdr lists) (append acc (car lists)))))

(defmacro mappend (function list &rest more-lists)
  `(append-each (mapcar ,function ,list ,@more-lists)))

(defmacro exp-aux (args)
  "Auxillary function for ex-X macros."
  (with-gensyms (arg)
    `(mappend #'(lambda (,arg)
		 (cond ((terminal-class? ,arg) (expand-terminal ,arg))
		       (t (list ,arg)))) ,args)))
;; These use weird unicode symbols as a gensymmy object code. This could be done
;; better. Also, if an input word is NIL everything breaks.

(defmacro exp-^ (args)
  "Given forms 1 2 3 ... in (^ 1 2 3 ...) expand it."
  `(cons '☧ (exp-aux ,args)))

(defmacro exp-+ (args)
  "Expand (+ 1 2 3 ...)"
  `(cons '➯ (exp-aux ,args)))

(defmacro exp-* (args)
  "Expand (* 1 2 3 ...)"
  `(cons '⅏ (exp-aux ,args)))

(defmacro exp-g (args)
  "Expand (* 1 2 3 ...)"
  `(cons '⚚ (exp-aux ,args)))

(defmacro defmatch (name symbol)
  "Create a true predicate to determine an expanded class's token."
  (with-gensyms (expanded)
    `(defpredicate ,name ,(list expanded)
       (and (listp ,expanded)
	    (and (symbolp (car ,expanded))
		 (eq (car ,expanded) ',symbol))))))

(defmatch g? ⚚)
(defmatch ^? ☧)
(defmatch *? ⅏)
(defmatch +? ➯)

(defmacro expand-correct (args)
  "Auxillary macro for expand. Used for subterminal (operation terminal terminal...)
   argument."
  (with-gensyms (once)
    `(let ((,once ,args))
       ;; Cond necessary because macros cannot be funcall'd
       (cond ((eql '^ (car ,once)) (exp-^ (cdr ,once)))
	     ((eql '+ (car ,once)) (exp-+ (cdr ,once)))
	     ((eql '* (car ,once)) (exp-* (cdr ,once)))
	     ((eql 'g (car ,once)) (exp-g (cdr ,once)))))))

(defun rule-class? (name)
  (some #'(lambda (x)
	    (eql (car x) name)) *rules*))

(defun read-expansion (rule)
  (labels ((read-expansion-aux (rules*)
	     (if (null rules*)
		 nil
		 (if (eq rule (caar rules*))
		     (cadar rules*)
		     (read-expansion-aux (cdr rules*))))))
    (read-expansion-aux *rules*)))

(defun expanded-p (rule)
  (not (null (read-expansion rule))))

(defun expand (rule)
  "Expand a rule."
  (cond ((null rule) nil)
	((rule-class? rule) (read-expansion rule))
	((terminal-class? rule)
	 (expand-terminal rule))
	((subterminal? rule)
	 (expand-correct rule))
	(t (expand-correct (cons (car rule)
				 (mapcar #'expand (cdr rule)))))))

(defun make-a-rule (name expansion)
  "Auxilliary function to defrule."
  (if (expanded-p name)
      nil
       (setf *rules*
	     (cons (list name expansion)
		   *rules*))))

(defmacro defrule (name &rest expansion)
  `(make-a-rule ',name (expand '(g ,@expansion))))

(defrule suffix (^ noun-suffix adverb-suffix adjective-suffix verb-suffix
		   oddball-suffixes))

;; English explaination of a polyword:
;; 1) Search if there is a prefix. If there isn't, it is optional, so move on.
;; 2) Search if the term is a member of the root terminal class. If it isn't,
;;    just return it. This will make any subsequent search be looking at an
;;    empty string "".
;; 3) Search if the last part of the term is 1 or more presuffixes followed by
;;    one or more suffixes. If not, search if it is one or more suffixes. If not
;;    , return the search term.
;; When the above returns a match, it will look like (class . "match"). When it
;; is forced to return raw data, it just looks like "$", where $ is the data.
;; The result is contained in a list, like ((class . "match") "$")

(defrule polyword (* prefix) (^ root) (^ (g (+ presuffix)
					    (+ suffix))
					 (+ suffix)))

(defrule correlative correlative-prefix correlative-suffix)

(defun match-terminal? (word terminal)
  "Partial predicate which returns whether the word matches the terminal symbol. 
   Returns either the match or NIL."
  (awhen (inside (symbol-name word) (symbol-name terminal))
    (intern it)))

(defun match-terminal-class? (word terminal-class)
  "Partial predicate built over match-terminal?"
  (some #'(lambda (terminal)
	    (match-terminal? word terminal)) terminal-class))

(defun operation (word class)
  "Run the function operation for each class type."
  (cond ((null class) '⅏)
	((atom class) (match-terminal? word class))
	((g? class)
	 (symb (operation (cadr class)) (operation (cddr class))))
	((^? sym)
	 )
	((*? sym))
	((+? sym))
	(t )))
