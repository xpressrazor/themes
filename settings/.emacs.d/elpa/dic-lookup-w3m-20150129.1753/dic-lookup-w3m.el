;;; dic-lookup-w3m.el --- look up dictionaries on the Internet

;; Copyright (C) 2008, 2009, 2010, 2011, 2012, 2014  mcprvmec

;; Author: mcprvmec
;; Keywords: emacs-w3m, w3m, dictionary
;; Package-Requires: ((w3m "20120723.324") (stem "20120826"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Look up in dictionaries on the Internet using emacs-w3m.

;;; Usage:
;; Requirements:
;; emacs-w3m
;; http://emacs-w3m.namazu.org/
;; w3m
;; http://w3m.sourceforge.net/
;;
;; Recommended:
;; stem.el in sdic
;; http://www.namazu.org/~tsuchiya/sdic/
;;
;; ~/.emacs:
;; ;;; w3mで辞書を引く
;; (autoload 'dic-lookup-w3m "dic-lookup-w3m" "w3mで辞書を引く" t)
;; or
;; (require 'dic-lookup-w3m)
;;
;; Key binding examples:
;; (global-set-key "\C-cd" 'dic-lookup-w3m)
;; (global-set-key "\C-cc" '(lambda () "excite 英和,和英" (interactive)
;; 			   (dic-lookup-w3m "ej-excite")))
;; (global-set-key "\C-ct" '(lambda () "nifty 英和翻訳" (interactive)
;; 			   (dic-lookup-w3m "tr-ej-nifty" 'sentence)))
;; (global-set-key "\C-cT" 'dic-lookup-w3m--tr-ej-yahoo-paragraph)
;;
;; Key translations:
;; w3mのバッファ内で
;; z     w3mバッファを隠して、w3mを起動する前のウインドウのレイアウトに戻す。
;; x     w3mを呼び出す前に表示していたウインドウを選択する。
;; f     w3mのfilterの有効、無効を切り替える。
;; F     発音記号のinline imageをフォントに変換して表示するかどうかを切
;;       り替える。
;; C-c l 英単語らしい文字列に辞書検索用のアンカーを付けるフィルタをトグルする。

;;; Code:

(require 'w3m)
(require 'w3m-search)
(require 'w3m-filter)
(eval-when-compile
  (require 'w3m-cookie))
(setq w3m-use-filter t)

(defconst dic-lookup-w3m-version "1.0"
  "Version string of this package.")

(defvar dic-lookup-w3m-config-files
  '(dic-lookup-w3m-ja
    dic-lookup-w3m-zh
    ;;dic-lookup-w3m-text-translator
    )
  "*辞書サイトの定義を記述したファイルのリスト。
dic-lookup-w3m.elがロードされたとき、このリストにあるシンボルがrequireされる。
シンボルは辞書サイトの定義ファイルでprovideしておく必要がある。
たとえば母語ごとに辞書サイトのセットを定義することを想定。
リストを記述する順番に注意。ファイルをロードする順番によって
`w3m-filter-rules'で定義されるフィルタの実行順序が変わる。同一サイトに
対するルールが複数のファイルに書かれている場合、ロードする順番で結果が
変わる可能性がある。")


(defvar dic-lookup-w3m-search-engine-alist '()
  "*検索エンジンのリスト。
最初の4個のメンバーは`w3m-search-engine-alist'参照。

5番目はその検索エンジンの説明。
6番目は`dic-lookup-w3m-suitable-engine'に渡す情報。
値はlistまたはsymbol。listのときは(query-pattern engine-regexp replacement)。
queryがQUERY-REGEXPに一致したら辞書engineの名前のENGINE-REGEXPにマッチ
した部分をREPLACEMENTに置き換えたものを新しい辞書エンジンにする。
symbolのときは関数名とみなして`dic-lookup-w3m-suitable-engine'が受け取った
引数search-engine query search-engine-alistで呼び出す。
関数は検索に使う辞書エンジン名を返さなければならない。")

(defvar dic-lookup-w3m-enable-search-engine-list nil
  "*使用する検索エンジンのリスト。
`w3m-search-engine-alist' にある検索エンジンのうち、使用した
いエンジンの名前を正規表現で記述する。完全に一致させるにはパターン
の前後に`^', `$'が必要。パターンのどれかにマッチした検索エンジンだ
けが`w3m-search-engine-alist'に追加され、辞書選択の補完候補に現れる。
nilなら`w3m-search-engine-alist'にあるすべてのエンジンを使う。")

(defvar dic-lookup-w3m-search-engine-aliases '()
  "*search engineの別名のリスト。
search engineを別の覚えやすい名前で登録する。
`((ALIAS ENGINE-NAME)..)")

(defvar dic-lookup-w3m-related-site-list '()
  "*queryに関連した検索をしやすくするために表示するサイトのリスト。
`(category
  ((search-engine . display-name)..))
 ..
`dic-lookup-w3m-filter-related-links'参照。")

(mapc 'require dic-lookup-w3m-config-files)

(dolist (elem dic-lookup-w3m-search-engine-aliases)
  (let ((engine (assoc (cadr elem) dic-lookup-w3m-search-engine-alist)))
    (if engine
	(add-to-list 'dic-lookup-w3m-search-engine-alist
		     `(,(car elem) ,@(cdr engine))))))

(dolist (elem dic-lookup-w3m-search-engine-alist)
  (if (or (null dic-lookup-w3m-enable-search-engine-list)
	  (assoc-default (car elem) dic-lookup-w3m-enable-search-engine-list
			 'string-match t))
      (add-to-list 'w3m-search-engine-alist elem)))

(defvar dic-lookup-w3m-autodef-func t
  "各search engineを呼び出すための関数を生成する。
non-nilなら各search enginごとにdic-lookup-w3m--ENGINNAME,
dic-lookup-w3m--ENGINENAME-region, dic-lookup-w3m--ENGINENAME-sentense
のような関数を自動生成する。キーバインドして使用することを想定。")

(if dic-lookup-w3m-autodef-func
    (dolist (elem dic-lookup-w3m-search-engine-alist)
      (when (or (null dic-lookup-w3m-enable-search-engine-list)
		(assoc-default (car elem)
			       dic-lookup-w3m-enable-search-engine-list
			       'string-match t))
	(fset (intern (format "dic-lookup-w3m--%s" (car elem)))
	      `(lambda (&optional query)
		 ,(format "Call `dic-lookup-w3m' with argment \"%s\".\n(See `dic-lookup-w3m-search-engine-alist')"
			  (car elem))
		 (interactive)
		 (funcall 'dic-lookup-w3m ,(car elem) query)))
	(dolist (thing '(word region line sentence paragraph buffer))
	  (fset (intern (format "dic-lookup-w3m--%s-%s" (car elem) thing))
		`(lambda ()
		   ,(format "Call `dic-lookup-w3m' with arguments \"%s\", '%s.\n(See `dic-lookup-w3m-search-engine-alist')"
			    (car elem) thing)
		   (interactive)
		   (funcall 'dic-lookup-w3m ,(car elem) ',thing)))
	  ))))

;; どのキーにもバインドされていない場合のみバインドする
(dolist (elem
	 '(("z" dic-lookup-w3m-bury-buffer) ; or rebind `q'
	   ("f" dic-lookup-w3m-toggle-filter)
	   ("x" dic-lookup-w3m-select-last-window)
	   ("F" dic-lookup-w3m-toggle-phonetic-image)
	   ("\C-cl" dic-lookup-w3m-filter-toggle-eword-anchor)))
  (unless (where-is-internal (cadr elem) w3m-mode-map)
    (apply 'define-key w3m-mode-map elem)))

(defun dic-lookup-w3m-select-last-window ()
  "w3mを呼び出す前に表示していたウインドウを選択する。"
  (interactive)
  (other-window -1))

(defvar dic-lookup-w3m-window-configuration nil
  "w3mから戻ったときに元のウインドウを表示するために覚えておく。")

(defun dic-lookup-w3m-bury-buffer ()
  "w3mバッファを隠して、`w3m'を起動する前のウインドウのレイアウトに戻す。"
  (interactive)
  (unless w3m-display-inline-images
    (w3m-toggle-inline-images 'turnoff))
  (if (window-configuration-p dic-lookup-w3m-window-configuration)
      (progn
	(set-window-configuration dic-lookup-w3m-window-configuration)
	(setq dic-lookup-w3m-window-configuration nil))
    (switch-to-buffer (other-buffer))))

;; w3m-search.elのw3m-search-read-variablesを変更
(defun dic-lookup-w3m-read-search-engine (&optional search-engine arg)
  (if (or (null search-engine)
	  (eq arg '-)
	  (and (numberp arg) (< arg 0))
	  (and (listp arg) (numberp (car arg)) (< (car arg) 0)))
      (let ((default (or (car w3m-search-engine-history)
			 w3m-search-default-engine))
	    (completion-ignore-case t))
	(completing-read (format "Which engine? (default %s): "
				 default)
			 w3m-search-engine-alist nil t nil
			 'w3m-search-engine-history default))
    (if (car (assoc search-engine w3m-search-engine-alist))
	search-engine
      (dic-lookup-w3m-read-search-engine nil arg))))

(defvar dic-lookup-w3m-read-query-prefix-arg-alist
  '((2 . line)				; C-u 2
    (4 . sentence)			; C-u
    (3 . region)			; C-u 3
    (16 . region)			; C-u C-u
    (5 . paragraph)			; C-u 5
    (64 . paragraph)			; C-u C-u C-u
    (6 . buffer)			; C-u 6
    (256 . buffer)			; C-u C-u C-u C-u
    )
  "*`dic-lookup-w3m'の前置引数とテキストの選択範囲の対応。")

;; w3m-search.elのw3m-search-read-variablesを変更
(defun dic-lookup-w3m-read-query (search-engine query &optional arg)
  (cond ((numberp arg)
	 (setq arg (abs arg)))
	((and (listp arg) (numberp (car arg)))
	 (setq arg (abs (car arg)))))
  (setq arg (assoc-default arg dic-lookup-w3m-read-query-prefix-arg-alist))
  (cond
   ((eq arg 'line)
    (setq query (thing-at-point 'line)))
   ((eq arg 'sentence)
    (setq query (thing-at-point 'sentence)))
   ((eq arg 'region)
    (setq query (buffer-substring (region-beginning) (region-end))))
   ((eq arg 'paragraph)
    (setq query (thing-at-point 'paragraph)))
   ((eq arg 'buffer)
    (setq query (thing-at-point 'buffer)))
   ((eq arg 'word)
    (setq query
	  (or (thing-at-point 'word)
	      (save-excursion
		(re-search-forward "\\S-" nil t) (thing-at-point 'word)))))
   ((eq query 'line)
    (setq query (thing-at-point 'line)))
   ((eq query 'sentence)
    (setq query (thing-at-point 'sentence)))
   ((eq query 'region)
    (setq query (buffer-substring (region-beginning) (region-end))))
   ((eq query 'paragraph)
    (setq query (thing-at-point 'paragraph)))
   ((eq query 'buffer)
    (setq query (thing-at-point 'buffer)))
   ((eq query 'word)
    (setq query
	  (or (thing-at-point 'word)
	      (save-excursion
		(re-search-forward "\\S-" nil t) (thing-at-point 'word)))))
   ((eq query nil)
    (setq query
	  (w3m-search-read-query
	   (format "%s search: " search-engine)
	   (format "%s search (default %%s): " search-engine))))
   (t query))

  (if (and query (string-match "\n" query)) ; 翻訳サイト用
      (while (string-match "^[ \t]+\\|[ \t]+$" query)
	(setq query (replace-match "" t nil query))))
  query)

;; w3m-search.elのw3m-search-escape-query-stringを修正
(defadvice w3m-search-escape-query-string
  (around do-not-modify-query-string-just-encode-it (str &optional coding))
  "query stringを空白文字で分割しない。"
  (setq ad-return-value
	(w3m-url-encode-string str (or coding w3m-default-coding-system))))

(defvar dic-lookup-w3m-buffer-name ""
  "*辞書の検索結果を表示するためのw3mバッファの名前。
non-nilなら、w3mセッションのバッファ名。複数のw3mセッションがある
とき、常にこのバッファで検索を行う。もしバッファ名のセッションが存
在しないときは新しいw3mセッションを開始して、そのバッファ名をこの
変数に保持する。
nilなら`w3m-goto-url'が選択するセッションで検索を行う。(w3mのデフォ
ルトの動作。)
二つのw3mセッションの片方に読みたいwebサイトを表示して、もう一方に
辞書を表示するという使い方を想定。")

(defvar dic-lookup-w3m-query "" "query string. 作業用一時データ")

;;;###autoload
(defun dic-lookup-w3m (&optional search-engine query)
  "w3mを使ってインターネット上の辞書を引く。または翻訳する。

search-engineがnon-nilならそのsearch-engineを使う。nilならミニバッ
ファから読み取る。search-engineは
`dic-lookup-w3m-search-engine-alist'に存在するもの。

queryが文字列ならその文字列をsearch-engineに送る。シンボルならカレ
ントバッファから文字列を選択してsearch-engineに送る。シンボルの値
によって選択範囲を指定する。指定できるシンボルはline, sentence,
region, paragraph, buffer。region以外は`thing-at-point'を使用して
文字列を選択する。queryがnilならミニバッファから読み取る。

前置引数を与えると、その値によって文字列の選択範囲を指定する。範囲
指定の種類はqueryに与えるシンボルと同じ。前置引数の値と範囲指定の
組み合わせは`dic-lookup-w3m-read-query-prefix-arg-alist'で指定する。
たとえばC-uでsentense、C-u C-uでregionなど。

前置引数はqueryの値に優先する。たとえばsentenceを指定した
dic-lookup-w3mの呼び出しをキーバインドしておき、場合によって前置引
数でregionやparagraphを指定するようにすると、キーバインドの数とキー
ストロークを節約できる。
queryへのシンボルの指定と前置引数の指定は翻訳サイトでの使用を想定
している。"
  (interactive)
  (setq search-engine
	(dic-lookup-w3m-read-search-engine search-engine current-prefix-arg))
  (setq query
	(dic-lookup-w3m-read-query search-engine query current-prefix-arg))
  (when query
    (unless			       ; looks like a sentence or more
	(string-match "[^ \t\n]+[ \t\n]+[^ \t\n]+[ \t\n]+[^ \t\n]" query)
      (setq dic-lookup-w3m-query query))
    (setq search-engine
	  (dic-lookup-w3m-suitable-engine search-engine query))
    (unless dic-lookup-w3m-window-configuration
      (setq dic-lookup-w3m-window-configuration
	    (current-window-configuration)))
    (when dic-lookup-w3m-buffer-name
      (pop-to-buffer (get-buffer dic-lookup-w3m-buffer-name))
      (unless (get-buffer dic-lookup-w3m-buffer-name)
	(w3m nil t)
	(setq dic-lookup-w3m-buffer-name (buffer-name))))
    (ad-activate 'w3m-search-escape-query-string)
    (w3m-search search-engine query)
    (ad-deactivate 'w3m-search-escape-query-string)))

(defun dic-lookup-w3m-last-engine (&optional query)
  "最後にミニバッファから読み込んで使用したサイトを使用して検索する。
キーバインドから起動したサイトは、最後にミニバッファから読み込んで
使用したサイトにならない。"
  (interactive)
  (dic-lookup-w3m (car w3m-search-engine-history) query))

(defun dic-lookup-w3m-suitable-engine (search-engine query)
  "適切な辞書に切り替える。
たとえば英和辞典で日本語を検索しようとした場合に和英辞典に切り替え
て検索する。切り替える規則は`dic-lookup-w3m-search-engine-alist'の
6番目の値を使用。値はリスト、シンボルまたは関数。リストな
ら(REGEXP FROM TO)。queryがREGEXPにマッチしたら、search-engineの正
規表現FROMにマッチした部分をTOに置換したエンジン名を返す。シンボル
ならその値をリストとして使用して同様に置換する。関数なら
search-engine, queryを引数として呼び出す。
"
  (let ((rule
	 (nth 5 (assoc search-engine dic-lookup-w3m-search-engine-alist))))
    (if (functionp rule)
	(funcall rule search-engine query)
      (if (symbolp rule)
	  (setq rule (symbol-value rule)))
      (if (consp rule)
	  (or
	   (and (string-match (car rule) query)
		(string-match (nth 1 rule) search-engine)
		(car
		 (assoc (replace-match (nth 2 rule) t nil search-engine)
			dic-lookup-w3m-search-engine-alist)))
	   search-engine)
	search-engine))))

(defvar dic-lookup-w3m-inline-image-rules '()
  "*w3mでinline imageを表示するかどうかをサイトごとに指定するリスト。
 ((REGEXP . FLAG) ...)
FLAGが'turnoffなら正規表現にマッチしたサイトではinline imageを表示しない。
turnoff, nil以外なら表示する。
FLAGがnilであるか、またはサイトがどの正規表現にもマッチしなかった場合は
`dic-lookup-w3m-inline-image-inherit'の指定に従う。
`w3m-toggle-inline-images'参照。")

(defvar dic-lookup-w3m-inline-image-inherit nil
  "*inline imageの表示/非表示切り替え規則。
inline imageの表示ルールが`dic-lookup-w3m-inline-image-rules'に定
義されていないサイトのデフォルトの動作の指定。
non-nilならinline imageの表示／非表示の状態を切り替えない。(その前
に表示したページと同じ。)
nilなら`w3m-default-display-inline-images'の値に従う。")

(defun dic-lookup-w3m-decide-inline-image ()
  "サイトごとにinline imageを表示するかどうかを切り替える。"
  (when (w3m-display-graphic-p)
    (let ((flag
	   (assoc-default w3m-current-url dic-lookup-w3m-inline-image-rules
			  'string-match)))
      (if flag
	  (w3m-toggle-inline-images flag)
	(unless dic-lookup-w3m-inline-image-inherit
	  (w3m-toggle-inline-images
	   (or w3m-default-display-inline-images 'turnoff)))))))

;;(add-hook 'w3m-display-hook 'dic-lookup-w3m-decide-inline-image)
(add-hook 'w3m-fontify-after-hook 'dic-lookup-w3m-decide-inline-image)

;; image animate emacs 24.1
;; 漢字の書き順サイト用
(when (fboundp 'image-animate)
  (add-hook 'w3m-display-hook
	    '(lambda (s)
	       (image-animate (image-get-display-property) 1 60)
	       )))

(defun dic-lookup-w3m-toggle-filter ()
  "w3mのfilterのon/offを切り替える。"
  (interactive)
  (setq w3m-use-filter (null w3m-use-filter))
  (w3m-redisplay-this-page)
  (if w3m-use-filter
      (w3m-message "w3m-filter is on.")
    (w3m-message "w3m-filter is off.")))

;; w3m-filter.elのw3m-filterを修正
(defadvice w3m-filter
  (around multi-filters (url))
  "Apply filtering rule of URL against a content in this buffer."
  (save-match-data
    (dolist (elem (append w3m-filter-rules
			  (delq nil
				(mapcar
				 (lambda (config)
				   (when (car config)
				     (if (consp (nth 3 config))
					 (cons (nth 2 config) (nth 3 config))
				       (list (nth 2 config) (nth 3 config)))))
				 w3m-filter-configuration))))
      (when (string-match (car elem) url)
	(if (listp (cadr elem))
	    (dolist (elem2 (cdr elem))
	      (apply (car elem2) url (cdr elem2)))
	  (apply (cadr elem) url (cddr elem)))))))

(ad-activate 'w3m-filter)

;; w3m-filter.elのw3m-filter-delete-regionsを修正
(defadvice w3m-filter-delete-regions
  (around exclude-matched-strings (url start end &optional exclude-s exclude-e
				       regexp-s regexp-e))
  "Delete regions surrounded with a START pattern and an END pattern."
  (goto-char (point-min))
  (let (p (i 0))
    (while (and (if regexp-s (re-search-forward start nil t)
		  (search-forward start nil t))
		(setq p (if exclude-s (match-end 0) (match-beginning 0)))
		(if regexp-e (re-search-forward end nil t)
		  (search-forward end nil t)))
      (delete-region p (if exclude-e (match-beginning 0) (match-end 0)))
      (+ i 1))
    (setq ad-return-value (> i 0))))

(ad-activate 'w3m-filter-delete-regions)

;; w3m-filter.elのw3m-filter-replace-regexpを修正
(defadvice w3m-filter-replace-regexp
  (around replace-match-without-case-conversion (url regexp to-string))
  "Replace all occurrences of REGEXP with TO-STRING."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-string t)))

(ad-activate 'w3m-filter-replace-regexp)

(defun dic-lookup-w3m-filter-word-anchor (url search-engine regexp subexp)
  "webページ中の文字列に対して、辞書検索用のアンカーを作成する。
regexpとsubexpで指定したパターンにマッチする文字列を検索語として
search-engineで指定した辞書に対してリンクを張る。"
  (let ((search-engine (if (symbolp search-engine)
			   (symbol-value search-engine)
			 search-engine)))
    (goto-char (point-min))
    (re-search-forward "<body[^>]+>\\|<body>" nil t)
    (goto-char (match-beginning 0))

    (while (re-search-forward regexp nil t)
      (replace-match
       (format
	"<a href=\"%s\">%s</a>"
	(format
	 (nth 1 (assoc search-engine w3m-search-engine-alist))
	 (w3m-url-encode-string
	  (match-string subexp)
	  (nth 2(assoc search-engine w3m-search-engine-alist))))
	(w3m-encode-specials-string (match-string subexp)))
      	))))

(defun dic-lookup-w3m-filter-eword-anchor (url search-engine &optional
					       min-length coding)
  "webページ中の英単語らしい文字列に対して、辞書検索用のアンカーを作成する。
min-lengthより短い文字列にはアンカーを作成しない。"
  (let ((search-engine (if (symbolp search-engine)
			   (symbol-value search-engine)
			 search-engine))
	(min-length (or min-length 3))
	s e (inhref nil) word)
    (goto-char (point-min))
    (re-search-forward "<body[^>]+>\\|<body>" nil t)
    (goto-char (match-beginning 0))
    (while (re-search-forward "\\(<[^>]+>\\)\\([^>]*\\)<" nil t)
      (setq s (match-beginning 2) e (match-end 2))
      (cond ((string-match "^<a[^a-z]" (match-string 1)) (setq inhref t))
	    ((string-match "^</a[^a-z]" (match-string 1)) (setq inhref nil)))
      (goto-char e)
      (unless inhref
	(save-excursion
	  (save-restriction
	    (narrow-to-region s e)
	    (goto-char (point-min))
	    (while (re-search-forward
		    (format
		     "\\([^-a-zA-Z'&]\\|\\`\\)\\([-a-zA-Z']\\{%s,\\}\\)"
		     min-length) nil t)
	      (setq word (match-string 2))
	      (delete-region (match-beginning 2) (match-end 2))
	      (insert
	       (format "<a href=\"%s\">%s</a>"
		       (format
			(nth 1 (assoc search-engine w3m-search-engine-alist))
			(w3m-url-encode-string word coding))
		       (w3m-encode-specials-string word)))
	      )))))))

(defvar dic-lookup-w3m-favorite-ej-engine "ej-excite"
  "*英単語からリンクを張るデフォルトのsearch engine.
`dic-lookup-w3m-filter-toggle-eword-anchor'参照。")

(defun dic-lookup-w3m-filter-toggle-eword-anchor (&optional flag)
  "英単語らしい文字列に辞書検索用のアンカーを付けるフィルタをトグルする。
引数なしで呼び出すとフィルタをトグルする。flagの値がturnoffならフィ
ルタを無効にする。turnoffでもnilでもなければフィルタを有効にする。
フィルタを有効にすると、ページ中にあるすべての英単語らしい文字列に
検索エンジン``dic-lookup-w3m-favorite-ej-engine''を検索するリンク
をつける。"
  (interactive)
  (let ((rule '("\\`\\(https?\\|file\\)://" dic-lookup-w3m-filter-eword-anchor
		dic-lookup-w3m-favorite-ej-engine)))
    (cond
     ((eq flag 'turnoff)
      (setq w3m-filter-rules (delete rule w3m-filter-rules)))
     ((not (null flag))
      (add-to-list 'w3m-filter-rules rule t))
     ((member rule w3m-filter-rules)
      (setq w3m-filter-rules (delete rule w3m-filter-rules)))
     (t
      (add-to-list 'w3m-filter-rules rule t)))
    (w3m-redisplay-this-page)))

(require 'url-parse)
(require 'url-util)
(defun dic-lookup-w3m-get-query-from-url (url baseurl &optional coding)
  "urlからquery文字列を取り出す。"
  (let (str)
    (if (string-match ".*\\?.*%s" baseurl)
	(let ((base-query
	       (replace-regexp-in-string
		".*\\?" "" (url-filename (url-generic-parse-url baseurl)) t))
	      (query
	       (replace-regexp-in-string
		".*\\?" "" (url-filename (url-generic-parse-url url)) t)))
	  (setq str
		(cadr
		 (assoc
		  (car (rassoc '("%s") (url-parse-query-string base-query)))
		  (url-parse-query-string query)))))
      (string-match "\\(.*\\)%s\\(.*\\)" baseurl)
      (if (string-match (format "%s\\([^&;?]+\\)%s"
				(regexp-quote (match-string 1 baseurl))
				(regexp-quote (match-string 2 baseurl)))
			url)
	  (setq str (match-string 1 url))))
    (if str
	(replace-regexp-in-string
	 "+" " " (w3m-url-decode-string str coding) t)
      "")))

(defvar dic-lookup-w3m-filter-do-show-candidates-heading " Possibly: "
  "*単語の候補リストの前に表示する見出し。
`dic-lookup-w3m-filter-show-candidates'参照。")

(defun dic-lookup-w3m-filter-show-candidates (url search-engine
						  &optional regexp before)
  "queryから英語の活用語尾を取り除いて見出し語の候補を表示する。
この機能を使うにはstem.elが必要。
stem.elはsdicに含まれています。またlookupにstem-english.elという名前で
含まれています。"
  (let* ((baseurl (nth 1 (assoc search-engine w3m-search-engine-alist)))
	 (coding (nth 2 (assoc search-engine w3m-search-engine-alist)))
	 (candidates (stem:stripping-suffix
		      (dic-lookup-w3m-get-query-from-url url baseurl coding)))
	 (candidates2 (stem:stripping-suffix dic-lookup-w3m-query)))
    (mapc #'(lambda (s) (setq candidates2 (delete s candidates2)))
	  candidates)
    (if (or candidates candidates2)
	(w3m-filter-replace-regexp
	 url
	 (concat "\\(" (or regexp "<body[^>]*>") "\\)")
	 (concat
	  (unless before "\\1")
	  "<span id=\"dic-lookup-w3m-candidates\">"
	  dic-lookup-w3m-filter-do-show-candidates-heading
	  (mapconcat
	   (lambda (s)
	     (format "<a href=\"%s\">%s</a>"
		     (format baseurl (w3m-url-encode-string s coding))
		     (w3m-encode-specials-string s)))
	   candidates ", ")
	  (if (and candidates candidates2) " | ")
	  (mapconcat
	   (lambda (s)
	     (format "<a href=\"%s\">%s</a>"
		     (format baseurl (w3m-url-encode-string s coding))
		     (w3m-encode-specials-string s)))
	   candidates2 ", ")
	  "</span><!-- /dic-lookup-w3m-candidates -->"
	  (if before "\\1")
	  )))
    ))

(condition-case nil
    (require 'stem)
  (error
   (fset 'dic-lookup-w3m-filter-show-candidates
	 '(lambda (url search-engine &optional regexp before)
	    "dummy. do nothing."))))

(defvar dic-lookup-w3m-filter-related-links-heading " Relevant: "
  "*関連サイトのリストの前に表示する見出し。")

(defun dic-lookup-w3m-filter-related-links
  (url search-engine category &optional baseurl coding regexp before)
  "queryに関連するリンクを表示する。
ある辞書で検索した単語を他の辞書でも簡単に検索できるように、他の辞
書へリンクを張る。"
  (let ((query (dic-lookup-w3m-get-query-from-url
		url
		(or baseurl
		    (nth 1 (assoc search-engine w3m-search-engine-alist)))
		(or coding
		    (nth 2 (assoc search-engine w3m-search-engine-alist)))))
	(site-list (cadr (assoc category dic-lookup-w3m-related-site-list))))
    (unless (equal query "")
      (w3m-filter-replace-regexp
       url
       (concat "\\(" (or regexp "<body[^>]*>") "\\)")
       (concat
	(unless before "\\1")
	"<span id=\"dic-lookup-w3m-related-links\">"
	dic-lookup-w3m-filter-related-links-heading
	(mapconcat
	 (lambda (s)
	   (if (assoc (car s) w3m-search-engine-alist)
	       (format
		"<a href=\"%s\">%s</a>"
		(format (nth 1 (assoc (car s) w3m-search-engine-alist))
			(w3m-url-encode-string
			 query
			 (nth 2 (assoc (car s) w3m-search-engine-alist))))
		(w3m-encode-specials-string (cdr s)))
	     (concat (car s) "??")))
	 (delete (assoc search-engine site-list) (copy-sequence site-list))
	 ", ")
	"</span><!-- /dic-lookup-w3m-related-links -->"
	(if before "\\1")
	)))))

(defun dic-lookup-w3m-filter-refresh-url (url new-url &optional regexp subexp)
  "htmlの<meta http-equiv=\"refresh\" ...>を使って新しいページに移動する。
辞書の見出し語の一覧のページから、最初の見出し語の説明のページに自
動的に移動するのに使う。"
  (goto-char (point-min))
  (w3m-filter-replace-regexp
   url
   "\\(<head[^>]*>\\)"
   (format
    "\\1<meta http-equiv=\"refresh\" content=\"0; url=%s\">"
    (format new-url
	    (or (and regexp (re-search-forward regexp nil t)
		     (match-string (or subexp 0)))
		"")))))

(defvar dic-lookup-w3m-filter-convert-phonetic-symbol t
  "*発音記号のinline imageをフォントに変換して表示するかどうかのフラグ。
non-nilなら、可能な場合はフォントに変換する。
nilならinline imageのまま。
`dic-lookup-w3m-toggle-phonetic-image'で使用。")

(defun dic-lookup-w3m-toggle-phonetic-image ()
  "発音記号のinline imageをフォントに変換して表示するかどうかを切り替える。
inline imageを表示すると時間がかかるためフォントに置き換える。
``w3m-filter''にフォントに変換するためのフィルターが定義されているサイ
トのみ有効。
フィルターは関数`dic-lookup-w3m-filter-convert-phonetic-symbol'を呼び
出すようになっていなければならない。"
  (interactive)
  (setq dic-lookup-w3m-filter-convert-phonetic-symbol
	(null dic-lookup-w3m-filter-convert-phonetic-symbol))
  (w3m-redisplay-this-page)
  (if dic-lookup-w3m-filter-convert-phonetic-symbol
      (w3m-message "display phonetic symbols using fonts.")
    (w3m-message "display phonetic symbols using inline images.")))

(defun dic-lookup-w3m-filter-convert-phonetic-symbol
  (url phonetic-symbol-table image-regexp &optional subexp)
  "発音記号などのinline imageをフォントに置き換える。
変数`dic-lookup-w3m-filter-convert-phonetic-symbol'がnilの場合は変
換しない。"
  (when dic-lookup-w3m-filter-convert-phonetic-symbol
    (goto-char (point-min))
    (while (re-search-forward image-regexp nil t)
      (let ((code (assoc-default
		   (match-string (or subexp 1))
		   (if (symbolp phonetic-symbol-table)
		       (symbol-value  phonetic-symbol-table)
		     phonetic-symbol-table))))
	(if code
	    (replace-match code t))))))

(define-minor-mode dic-lookup-w3m-mode
  "Toggle dic-lookup-w3m mode.
See the command \\[dic-lookup-w3m]."
  nil
  " dic-w3m"
  '()
  :group 'dic-lookup-w3m)

;;;###autoload
(defun dic-lookup-w3m-search-engine-menu (arg)
  "search engineの一覧を表示する。
表示は``dic-lookup-w3m-search-engine-alist''に出現する順。
C-uで名前でソート、C-u C-uで説明でソート。"
  (interactive "p")
  (let ((buffer (get-buffer-create " *dic-lookup-w3m-work*")))
    (save-current-buffer
      (set-buffer buffer)
      (delete-region (point-min) (point-max))
      (insert
       "<html><head><title>search engine list</title></head><body><table>\n")
      (mapc
       #'(lambda (e)
	  (insert
	   (format "<tr><td><a href=\"%s\">%s</a></td><td>%s</td></tr>\n"
		   (if (string-match "%s" (cadr e))
		       (replace-match "" t nil (cadr e))
		     (cadr e))
		   (car e)
		   (or (nth 4 e) ""))))
       (cond
	((eq arg 4)
	 (sort (copy-sequence dic-lookup-w3m-search-engine-alist)
	       #'(lambda (a b) (string< (car a) (car b)))))
	((eq arg 16)
	 (sort (copy-sequence dic-lookup-w3m-search-engine-alist)
	       #'(lambda (a b) (string< (nth 4 a) (nth 4 b)))))
	(t (reverse dic-lookup-w3m-search-engine-alist))))
      (insert "</table></body></html>\n"))
    (ad-activate 'w3m-about)
    (w3m-gohome)
    (w3m-redisplay-this-page)))

;;;###autoload
(defun dic-lookup-w3m-txt2html (&optional search-engine min-length)
  "カレントバッファのテキストを簡易なhtmlに変換する。
バッファ内の各英単語らしい文字列から辞書へのリンクを張る。
変換結果はemacsの新規バッファに出力する。ファイルに保存して別のwebブラ
ウザで表示出来る。"
  (interactive)
  (let ((org-buffer (current-buffer))
	(search-engine (dic-lookup-w3m-read-search-engine search-engine)))
    (find-file
     (concat
      (make-temp-name (expand-file-name "dic-lookup-w3m-")) ".html"))
    (delete-region (point-min) (point-max))
    (insert-buffer-substring org-buffer)
    (dic-lookup-w3m-htmlize search-engine (buffer-name) min-length)))

(defvar dic-lookup-w3m-temp-buffer " *dic-lookup-w3m-work*" "temp buffer")

;;;###autoload
(defun dic-lookup-w3m-txt2w3m (&optional search-engine min-length)
  "カレントバッファのテキストを簡易なhtmlに変換してw3mで開く。
バッファ内の各英単語から辞書へのリンクを張る。"
  (interactive)
  (let ((oldbuf (current-buffer)))
    (save-current-buffer
      (set-buffer (get-buffer-create dic-lookup-w3m-temp-buffer))
      (delete-region (point-min) (point-max))
      (insert-buffer-substring oldbuf)
      (dic-lookup-w3m-htmlize
       (dic-lookup-w3m-read-search-engine search-engine)
       (buffer-name) min-length))
    (ad-activate 'w3m-about)
    (w3m-gohome)
    (w3m-redisplay-this-page)
    ;;(ad-deactivate 'w3m-about)
    ))

;; w3m.elのw3m-aboutを修正
(defadvice w3m-about
  (around override (url &rest args))
  (insert-buffer-substring dic-lookup-w3m-temp-buffer)
  (setq ad-return-value "text/html"))

;;(ad-activate 'w3m-about)

(defun dic-lookup-w3m-htmlize (baseurl &optional buffer-name min-length)
  (goto-char (point-min))
  (insert
   (w3m-encode-specials-string (buffer-substring (point-min) (point-max))))
  (delete-region (point) (point-max))
  (goto-char (point-min))
  (while (re-search-forward "\n" nil t)
    (replace-match "<br>\n" t))
  (goto-char (point-min))
  (insert "<html><head><title>"
	  (w3m-encode-specials-string (or buffer-name (buffer-name)))
	  "</title></head><body><p>")
  (goto-char (point-max))
  (insert "</p></body></html>")
  (dic-lookup-w3m-filter-eword-anchor "" baseurl min-length))

(defvar dic-lookup-w3m-morpheme-cmd "c:/Program Files/MeCab/bin/mecab"
  "*形態素解析エンジンのコマンド。")
(defvar dic-lookup-w3m-morpheme-args
  '("-b81920" "--eos-format="
    "--node-format=%m\t%f[7]\t%f[6]\t%F-[0,1,2,3]\t%f[4]\t%f[5]\n"
    "--unk-format=%m\t%m\t%m\t%F-[0,1,2,3]\t\t\n"
    "--eos-format=EOS\n")
  "*形態素解析エンジンの引数。")
(defvar dic-lookup-w3m-morpheme-coding-system 'shift_jis-dos ;'euc-jp-unix
  "*形態素解析エンジンの文字コード。")
(defvar dic-lookup-w3m-morpheme-eos "EOS"
  "*形態素解析エンジンの出力の文末表示文字列。")

;; (defvar dic-lookup-w3m-morpheme-cmd "c:/Program Files/ChaSen/chasen.exe"
;;   "*形態素解析エンジンのコマンド。")
;; (defvar dic-lookup-w3m-morpheme-args '()
;;   "*形態素解析エンジンの引数。")
;; (defvar dic-lookup-w3m-morpheme-coding-system 'shift_jis
;;   "*形態素解析エンジンの文字コード。")
;; (defvar dic-lookup-w3m-morpheme-eos "EOS"
;;   "*形態素解析エンジンの出力の文末表示文字列。")

;;;###autoload
(defun dic-lookup-w3m-jtxt2w3m (&optional search-engine query)
  "日本語のテキストを、各単語に辞書へのリンクを付けたhtmlに変換してw3mで開く。
ミニバッファに入力したテキストまたはカレントバッファのテキストを簡
易なhtmlに変換してw3mで開く。日本語の各単語から辞書へリンクを張る。
辞書は国語辞典のほか和英や日中辞典など日本語が見出し語になっていれば使える。
`dic-lookup-w3m'と同じように、前置引数で変換するテキストの範囲を指
定できる。前置引数なしで、queryが空の場合はカレントバッファ全体を
変換する。
形態素解析エンジン(MeCab, ChaSen)の設定を
`dic-lookup-w3m-morpheme-cmd'、`dic-lookup-w3m-morpheme-args'、
`dic-lookup-w3m-morpheme-coding-system'、
`dic-lookup-w3m-morpheme-eos'でおこなう。
適切な形態素解析器があれば日本語以外の言語にも使える。"
  (interactive)
  (let* ((search-engine
	  (dic-lookup-w3m-read-search-engine search-engine current-prefix-arg))
	 (query
	  (dic-lookup-w3m-read-query search-engine query current-prefix-arg))
	 (engine (nth 1 (assoc search-engine w3m-search-engine-alist)))
	 (coding (nth 2 (assoc search-engine w3m-search-engine-alist)))
	 (org-buffer (current-buffer))
	 (src-tmp (get-buffer-create " *dic-lookup-w3m-work1*"))
	 (morpheme-out (get-buffer-create " *dic-lookup-w3m-work2*"))
	 (html-tmp (get-buffer-create dic-lookup-w3m-temp-buffer))
	 (morpheme))
    (with-current-buffer morpheme-out
      (delete-region (point-min) (point-max)))
    (with-current-buffer src-tmp
      (delete-region (point-min) (point-max))
      (if (equal query "")
	  (insert-buffer-substring org-buffer)
	(insert query))
      (goto-char (point-min))
      (while (re-search-forward "^[ 　\t]+\\|[ 　\t]+$" nil t)
	(replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\n\\(.\\)" nil t)
	(replace-match " \\1"))
      (goto-char (point-min))
      (while (re-search-forward "\\(\\cj\\) +\\(\\cj\\)" nil t)
	(replace-match "\\1\\2"))
      (goto-char (point-min))
      (while (re-search-forward "。" nil t)
	(replace-match "。\n"))
      (let ((coding-system-for-write dic-lookup-w3m-morpheme-coding-system)
	    (coding-system-for-read dic-lookup-w3m-morpheme-coding-system))
	(apply
	 'call-process-region
	 (point-min) (point-max)
	 dic-lookup-w3m-morpheme-cmd nil morpheme-out nil
	 dic-lookup-w3m-morpheme-args)))
    (with-current-buffer html-tmp
      (delete-region (point-min) (point-max))
      (if (equal query "")
	  (insert-buffer-substring org-buffer)
	(insert query))
      (goto-char (point-min)))
    (with-current-buffer morpheme-out
      (goto-char (point-min))
      (while (re-search-forward "^.+$" nil t)
	(unless (equal (match-string 0) dic-lookup-w3m-morpheme-eos)
	  (setq morpheme (split-string (match-string 0) "\t"))
	  (with-current-buffer html-tmp
	    (if (and
		 (re-search-forward
		  (mapconcat
		   #'(lambda (c)
		      (regexp-quote (string c)))
		   (car morpheme) "\\([ 　\t\n]\\)*")
		  nil t)
		 (not (equal (nth 2 morpheme) "")))
		(replace-match
		 (format
		  "<a href=\"%s\">%s</a>"
		  (format
		   engine
		   (save-match-data
		     (w3m-url-encode-string (nth 2 morpheme) coding)))
		  (save-match-data
		    (w3m-encode-specials-string (match-string 0))))
		 t))))))
    (with-current-buffer html-tmp
      (goto-char (point-min))
      (insert
       "<html><head><title>"
       (w3m-encode-specials-string
	(if (equal query "")
	    (buffer-name org-buffer)
	  (with-current-buffer src-tmp
	    (goto-char (point-min))
	    (re-search-forward "^.+$" nil t)
	    (buffer-substring (match-beginning 0) (min 17 (match-end 0))))))
       "</title></head><body><pre>\n")
      (goto-char (point-max))
      (insert "</pre></body></html>\n"))
    (ad-activate 'w3m-about)
    (w3m-gohome)
    (w3m-redisplay-this-page)))

(defun dic-lookup-w3m-next-anchor-line ()
  "ポイントのある行より後の行の最初のアンカーにポイントを移動する。
一行に複数のアンカーがあっても最初のひとつにしか移動しないので、遠くに
あるアンカーに早くたどり着ける。`dic-lookup-w3m-previous-anchor-line'も参照。"
  (interactive)
  (end-of-line)
  (w3m-next-anchor))

(defun dic-lookup-w3m-previous-anchor-line ()
  "ポイントのある行より前の行の最後のアンカーにポイントを移動する。
一行に複数のアンカーがあっても最初のひとつにしか移動しないので、遠くに
あるアンカーに早くたどり着ける。`dic-lookup-w3m-next-anchor-line'も参照。"
  (interactive)
  (beginning-of-line)
  (w3m-previous-anchor))

(provide 'dic-lookup-w3m)

(defvar dic-lookup-w3m-load-hook nil
  "*Hook run after loading the dic-lookup-w3m module.")

(run-hooks 'dic-lookup-w3m-load-hook)

;;; dic-lookup-w3m.el ends here
