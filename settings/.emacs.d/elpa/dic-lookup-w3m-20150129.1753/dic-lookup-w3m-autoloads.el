;;; dic-lookup-w3m-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "dic-lookup-w3m" "dic-lookup-w3m.el" (21787
;;;;;;  29777 160430 7000))
;;; Generated autoloads from dic-lookup-w3m.el

(autoload 'dic-lookup-w3m "dic-lookup-w3m" "\
w3mを使ってインターネット上の辞書を引く。または翻訳する。

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
している。

\(fn &optional SEARCH-ENGINE QUERY)" t nil)

(autoload 'dic-lookup-w3m-search-engine-menu "dic-lookup-w3m" "\
search engineの一覧を表示する。
表示は``dic-lookup-w3m-search-engine-alist''に出現する順。
C-uで名前でソート、C-u C-uで説明でソート。

\(fn ARG)" t nil)

(autoload 'dic-lookup-w3m-txt2html "dic-lookup-w3m" "\
カレントバッファのテキストを簡易なhtmlに変換する。
バッファ内の各英単語らしい文字列から辞書へのリンクを張る。
変換結果はemacsの新規バッファに出力する。ファイルに保存して別のwebブラ
ウザで表示出来る。

\(fn &optional SEARCH-ENGINE MIN-LENGTH)" t nil)

(autoload 'dic-lookup-w3m-txt2w3m "dic-lookup-w3m" "\
カレントバッファのテキストを簡易なhtmlに変換してw3mで開く。
バッファ内の各英単語から辞書へのリンクを張る。

\(fn &optional SEARCH-ENGINE MIN-LENGTH)" t nil)

(autoload 'dic-lookup-w3m-jtxt2w3m "dic-lookup-w3m" "\
日本語のテキストを、各単語に辞書へのリンクを付けたhtmlに変換してw3mで開く。
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
適切な形態素解析器があれば日本語以外の言語にも使える。

\(fn &optional SEARCH-ENGINE QUERY)" t nil)

;;;***

;;;### (autoloads nil nil ("dic-lookup-w3m-ja.el" "dic-lookup-w3m-pkg.el"
;;;;;;  "dic-lookup-w3m-text-translator.el" "dic-lookup-w3m-zh.el")
;;;;;;  (21787 29777 279680 769000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dic-lookup-w3m-autoloads.el ends here
