;;; dic-lookup-w3m-ja.el --- look up dictionaries on the Internet

;; Copyright (C) 2008, 2009, 2010, 2011, 2012, 2014, 2015  mcprvmec

;; Author: mcprvmec

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

;;; Code:

(defvar dic-lookup-w3m-search-engine-alist '())

(defun dic-lookup-w3m-search-engine-postget (list)
  (append
   list
   (mapcar
    #'(lambda (elem)
       (append
	(list (concat (nth 0 elem) "-get")
	      (concat (nth 1 elem) "?" (nth 3 elem))
	      (nth 2 elem))
	(if (nth 4 elem)
	    (append '(nil) (nthcdr 4 elem)))))
    list)))

(eval-when-compile
  (require 'w3m-search)
  (if (< max-specpdl-size 2000)
      (setq max-specpdl-size 2000))
  (if (< max-lisp-eval-depth 1000)
      (setq max-lisp-eval-depth 1000)))

(mapc
 #'(lambda (elem) (add-to-list 'dic-lookup-w3m-search-engine-alist elem))
 `(
   ;; yahoo dtype; 0:国語, 1:英和, 2:すべての辞書, 3:和英, 5:類語
   ("ej-yahoo"
    "http://dic.search.yahoo.co.jp/dsearch?p=%s&dic_id=ejje&stype=prefix&b=1"
    utf-8 nil "プログレッシブ和英中辞典(第３版), プログレッシブ英和中辞典(第４版)"
    dic-lookup-w3m-suitable-engine-pattern)
   ("je-yahoo"
    "http://dic.search.yahoo.co.jp/dsearch?p=%s&dic_id=ejje&stype=prefix&b=1"
    utf-8 nil "プログレッシブ和英中辞典(第３版), プログレッシブ英和中辞典(第４版)"
    dic-lookup-w3m-suitable-engine-pattern)
   ("jj-yahoo"
    "http://dic.search.yahoo.co.jp/dsearch?p=%s&dic_id=jj&stype=prefix&b=1"
    utf-8 nil "デジタル大辞泉, 大辞林 第三版")
   ("etc-yahoo"
    "http://dic.search.yahoo.co.jp/dsearch?p=%s&dic_id=etc&stype=prefix&b=1"
    utf-8 nil "その他の辞典")
   ("all-yahoo"
    "http://dic.search.yahoo.co.jp/search?p=%s&dic_id=all&stype=prefix&b=1"
    utf-8 nil "すべての辞書")

   ;; excite
   ("ej-excite" "http://www.excite.co.jp/dictionary/english_japanese/?search=%s"
    utf-8 nil "新英和中辞典第６版、新和英中辞典第４版（研究社）")
   ("jj-excite" "http://www.excite.co.jp/dictionary/japanese/?search=%s"
    utf-8 nil "大辞林第二版（三省堂）")
   ("cj-excite" "http://www.excite.co.jp/dictionary/chinese_japanese/?search=%s"
    utf-8 nil "デイリーコンサイス中日辞典（三省堂）")
   ("jc-excite" "http://www.excite.co.jp/dictionary/japanese_chinese/?search=%s"
    utf-8 nil "デイリーコンサイス日中辞典（三省堂）")
   ("ej-computer-excite"
    "http://www.excite.co.jp/dictionary/english_japanese/?dictionary=COMP_EJ&search=%s"
    utf-8 nil "英和コンピュータ用語辞典")
   ("je-computer-excite"
    "http://www.excite.co.jp/dictionary/english_japanese/?dictionary=COMP_EJ&search=%s"
    utf-8 nil "英和コンピュータ用語辞典")


   ;; ALC
   ("ej-alc" "http://eow.alc.co.jp/%s/UTF-8/" utf-8 nil "英辞郎")
   ("ej-alc-business-put" "http://home.alc.co.jp/db/owa/bdicn_sch" utf-8
    "w=%s" "ビジネス英語辞書")
   ("ej-alc-gogen-put" "http://home.alc.co.jp/db/owa/etm_sch" shift_jis
    "instr=%s&stg=1" "語源辞典")
   ("ej-alc-business"
    "http://home.alc.co.jp/db/owa/bdicn_sch?w=%s"
    utf-8 nil "ビジネス英語辞書")
   ("ej-alc-gogen" "http://home.alc.co.jp/db/owa/etm_sch?instr=%s&stg=1"
    shift_jis nil "語源辞典")

   ;; webster 英英
   ("ee-webster" "http://www.merriam-webster.com/dictionary/%s"
    utf-8 nil "Merriam-Webster Collegiate Dictionary")
   ("thesaurus-webster" "http://www.merriam-webster.com/thesaurus/%s"
    utf-8 nil "Merriam-Webster Collegiate Thesaurus")

   ;; cambridge 英英
   ("ee-cambridge" "http://dictionary.cambridge.org/search/british/direct/?q=%s"
    nil nil "Cambridge Advanced Learner's Dictionary")

   ;; longman 英英
   ("ee-longman" "http://www.ldoceonline.com/search/?q=%s" utf-8 nil
    "Longman Dictionary of Contemporary English & Longman Advanced American Dictionary")

   ;; oxford 英英
   ("ee-oxford"
    "http://www.oup.com/oald-bin/web_getald7index1a.pl?search_word=%s"
    utf-8 nil "Oxford Advanced Learner's Dictionary")

   ;; macmillan 英英
   ("ee-macmillan"
    "http://www.macmillandictionary.com/dictionary/british/%s"
    utf-8 nil "MACMILLAN Dictionary")

   ;; onelook 英英
   ("ee-onelook" "http://www.onelook.com/?w=%s&ls=a"
    nil nil "約1000の辞書を一括検索")

   ;; dict.org
   ("ee-dict.org"
    "http://www.dict.org/bin/Dict?Form=Dict1&Query=%s&Strategy=*&Database=*"
    nil nil "The DICT Development Group")
   ("ee-dict.org-post" "http://www.dict.org/bin/Dict" nil
    "Form=Dict1&Query=%s&Strategy=*&Database=*" "The DICT Development Group")

   ;; yahoo.com
   ("ee-yahoo.com"
    "http://education.yahoo.com/reference/dictionary/?s=%s" nil nil
    "The American Heritage® Dictionary of the English Language, Fourth Edition.")
   ("enes-yahoo.com"
    "http://education.yahoo.com/reference/dict_en_es/?s=%s" nil nil
    "The American Heritage® Spanish Dictionary: Spanish/English, Inglés/Español")
   ("esen-yahoo.com"
    "http://education.yahoo.com/reference/dict_en_es/?s=%s" nil nil
    "The American Heritage® Spanish Dictionary: Spanish/English, Inglés/Español")

   ;; BNC corpus
   ;; http://www.natcorp.ox.ac.uk/tools/chapter4.xml.ID=FIMNU#CQL
   ("corpus-bnc" "http://bnc.bl.uk/saraWeb.php?qy=%s&mysubmit=Go"
    nil nil "The British National Corpus (BNC)")

   ;; collins corpus
   ;; A query is made up of one or more terms concatenated with a + symbol.
   ;; dog+4bark "dog" followed by "bark" with up to 4 words intervening
   ;; blew@+away the set of words blow blows blowing blew followed by
   ;; the word away
   ;; cut* "cut", "cuts" and "cutting". probably a bad idea.
   ;; cut|cuts|cutting match an explicit set of words.
   ;; wordform/part-of-speech
   ;; (fool|fools|fooling|fooled)/VERB grouping
   ;; rather+JJ word "rather" is immediately followed by an adjective.
   ("corpus-collins"
    "http://www.collins.co.uk/Corpus/CorpusPopUp.aspx?query=%s&corpus=ukephem+ukmags+bbc+ukbooks+times+today+usbooks+npr+usephem+ukspok&width=100"
    nil nil "Collins Cobuild Corpus")

   ;; EReK corpus 英語のウェブページをコーパスとみなして検索する
   ("corpus-erek" "http://erek.ta2o.net/news/%s.html" utf-8 nil
    "英語で書かれたウェブページのテキストを巨大な例文集（コーパス）とみなして検索する")
   ("corpus-j-jrek" "http://jrek.ta2o.net/s/%s.html" utf-8 nil
    "日本語のウェブページのテキストを巨大な例文集（コーパス）とみなして検索する")

   ;; Dictionary.com
   ("thesaurus-rogets" "http://thesaurus.reference.com/browse/%s?jss=0"
    nil nil "Roget's 21st Century Thesaurus, Third Edition")
   ("ee-dictionrary.com" "http://dictionary.reference.com/browse/%s?jss=0"
    nil nil "English-English")

   ;; Visual Thesaurus thesaurus
   ("thesaurus-visualthesaurus"
    "http://www.visualthesaurus.com/browse/en/%s" nil nil
    "The Visual Thesaurus is an online thesaurus and dictionary of over 145,000 words")

   ;; kotonoha 日本語コーパス
   ;; (setq w3m-use-cookies t)が必要。さらに検索前に一度
   ;; http://www.kotonoha.gr.jp/shonagon/search_form を開く
   ("corpus-j-kotonoha"
    "http://www.kotonoha.gr.jp/shonagon/search_result?query_string=%s&&media=書籍&media=雑誌&media=新聞&media=白書&media=教科書&media=広報紙&media=Yahoo!知恵袋&media=Yahoo!ブログ&media=韻文&media=法律&media=国会会議録&entire_period=1"
    utf-8 nil "KOTONOHA 現代書き言葉均衡コーパス")

   ;; 青空文庫 日本語用例検索
   ("corpus-j-aozora" "http://www.tokuteicorpus.jp/team/jpling/kwic/search.cgi"
    shift_jis "cgi=1&sample=0&mode=1&kw=%s" "青空文庫 日本語用例検索")

   ;; 格フレーム検索 http://nlp.kuee.kyoto-u.ac.jp/nl-resource/caseframe.html
   ("corpus-j-caseframe" "http://reed.kuee.kyoto-u.ac.jp/cf-search/"
    euc-jp "text=%s"
    "格フレーム検索; 用言とそれに関係する名詞を用言の各用法ごとに整理したもの")
   ("corpus-j-caseframe-get"
    "http://reed.kuee.kyoto-u.ac.jp/cf-search/?text=%s" euc-jp nil
    "格フレーム検索; 用言とそれに関係する名詞を用言の各用法ごとに整理したもの")

   ;; Weblio
   ("thesaurus-j-weblio" "http://thesaurus.weblio.jp/content/%s" utf-8 nil
    "Weblio 約650000語の類語や同義語・関連語とシソーラスを収録")
   ("ej-weblio" "http://ejje.weblio.jp/content/%s" utf-8 nil
    "研究社新英和中辞典")
   ("je-weblio" "http://ejje.weblio.jp/content/%s" utf-8 nil
    "研究社新和英中辞典")
   ("jj-weblio" "http://www.weblio.jp/content/%s" utf-8 nil
    "三省堂デイリーコンサイス国語辞典他")
   ("cj-weblio" "http://cjjc.weblio.jp/content/%s" utf-8 nil
    "中日・日中辞典")
   ("jc-weblio" "http://cjjc.weblio.jp/content/%s" utf-8 nil
    "中日・日中辞典")
   ("jj-kobun-weblio" "http://kobun.weblio.jp/content/%s"
    utf-8 nil "古語辞典")
   ("shuwa-weblio" "http://shuwa.weblio.jp/content/%s"
    utf-8 nil "手話辞典")
   ("collocation-weblio" "http://ejje.weblio.jp/concordance/content/%s"
    utf-8 nil "英語共起表現")
   ("thesaurus-weblio" "http://ejje.weblio.jp/english-thesaurus/content/%s"
    utf-8 nil "英語シソーラス")
   ("corpus-weblio" "http://ejje.weblio.jp/sentence/content/%s"
    utf-8 nil "英語例文")

   ;; LSD Life Science Dictionary project
   ("ej-lsd" "http://lsd-project.jp/weblsd/begin/%s" utf-8 nil
    "Life Science Dictionary")
   ("corpus-lsd" "http://lsd-project.jp/weblsd/conc2/%s" utf-8 nil
    "Life Science Dictionary project corpus")
   ("thesaurus-lsd"
    "http://lsd-project.jp/cgi-bin/lsdproj/draw_tree.pl?opt=c&query=%s"
    utf-8 nil "Life Science Dictionary project シソーラス")
   ("tr-ej-lsd" "http://lsd.pharm.kyoto-u.ac.jp/cgi-bin/lsdproj/etoj-cgi04.pl"
    shift_jis "query=%s&lang=japanese&DIC=LSD"
    "Life Science Dictionary project 翻訳")

   ;; RNN 時事英語辞典
   ("ej-jijieigo" "http://rnnnews.jp/search/result/?q=%s" euc-jp nil
    "RAPID NEWS NETWORK 時事英語辞典")
   ("je-jijieigo" "http://rnnnews.jp/search/result/?q=%s" euc-jp nil
    "RAPID NEWS NETWORK 時事英語辞典")

   ;; FOKS Forgiving Online Kanji Search
   ;; 読み方のわからない熟語の読みを（不正確でも）いれて、正しい読みを
   ;; 調べられます。
   ("kanji-foks" "http://foks.info/search/?query=%s&action=Search" utf-8 nil
    "FOKS Forgiving Online Kanji Search 漢字")

   ;; babylon
   ("ej-babylon" "http://www.babylon.com/definition/%s/Japanese" utf-8 nil
    "バビロン")
   ("je-babylon" "http://www.babylon.com/definition/%s/Japanese" utf-8 nil
    "バビロン")
   ("jj-babylon" "http://www.babylon.com/definition/%s/Japanese" utf-8 nil
    "バビロン(Wikipedia)")
   ("ee-babylon" "http://www.babylon.com/definition/%s/English" utf-8 nil
    "バビロン")

   ;; infoseek
   ("ej-infoseek"
    "http://dictionary.infoseek.ne.jp/search/result?q=%s&t=0&r=ejje"
    utf-8 nil "プログレッシブ英和中辞典(第４版)")
   ("je-infoseek"
    "http://dictionary.infoseek.ne.jp/search/result?q=%s&t=0&r=ejje"
    utf-8 nil "プログレッシブ和英中辞典(第３版)")
   ("jj-infoseek"
    "http://dictionary.infoseek.ne.jp/search/result?q=%s&t=0&r=lang"
    utf-8 nil "デジタル大辞泉")
   ("jj-etc-infoseek"
    "http://dictionary.infoseek.ne.jp/search/result?q=%s&t=0&r=etc"
    utf-8 nil "その他の事典")

   ;; kotobank
   ("jj-kotobank" "http://kotobank.jp/search/result?q=%s"
    utf-8 nil "複数辞書検索 デジタル大辞泉, マイペディア, 知恵蔵, etc.")
   ("ej-kotobank" "http://kotobank.jp/ejsearch/result?q=%s"
    utf-8 nil "プログレッシブ英和中辞典(第４版)")
   ("je-kotobank" "http://kotobank.jp/ejsearch/result?q=%s"
    utf-8 nil "プログレッシブ和英中辞典(第３版)")

   ;; 漢字の書き順
   ;; 書き順でGO Alpha.Inc
   ;;("kanji-kakijun" "http://www.winttk.com/kakijun/dbf/profile.cgi"
   ;; shift_jis "key=%s&hor=1&max=1" "漢字の書き順")
   ("kanji-kakijun-alphainc"
    "http://www.google.co.jp/search?q=「%s」の書き順と書き方+site:www.kkjn.jp&lr=lang_ja&ie=UTF-8&oe=UTF-8"
    utf-8 nil "漢字の書き順")

   ;; 漢字ひつじゅん君
   ("kanji-kakijun-hitsujunkun"
    "http://www.google.co.jp/search?q=site:www.human.gr.jp/hitsujun+%s&ie=UTF-8&oe=UTF-8"
    utf-8 nil "小学校で学習する漢字の書き順")
    
   ;; 正しい漢字の書き順
   ("kanji-kakijun-kakijun.jp" "http://kakijun.jp/main/u_kensaku.cgi?KANJI=%s"
    utf-8 nil "正しい漢字の書き順")

   ;; goo
   ("ej-goo" "http://dictionary.goo.ne.jp/srch/ej/%s/m0u/"
    utf-8 nil "小学館 プログレッシブ英和中辞典 第4版")
   ("je-goo" "http://dictionary.goo.ne.jp/srch/je/%s/m0u/"
    utf-8 nil "小学館 プログレッシブ和英中辞典 第3版")
   ("jj-goo" "http://dictionary.goo.ne.jp/srch/jn/%s/m0u/"
    utf-8 nil "小学館 デジタル大辞泉")
   ("jj-yojijukugo-goo" "http://dictionary.goo.ne.jp/srch/idiom/%s/m0u/"
    utf-8 nil "三省堂 新明解四字熟語辞典")
   ("it-goo" "http://dictionary.goo.ne.jp/srch/it/%s/m0u/"
    utf-8 nil "IT用語辞典")
   ("all-goo" "http://dictionary.goo.ne.jp/srch/all/%s/m0u/"
    utf-8 nil "すべての辞書")
   ("thesaurus-j-goo" "http://dictionary.goo.ne.jp/srch/thsrs/%s/m0u/"
    utf-8 nil "小学館 使い方の分かる　類語例解辞典 新装版")
   ("cj-goo" "http://dictionary.goo.ne.jp/srch/cj/%s/m0u/"
    utf-8 nil "三省堂 デイリーコンサイス中日辞典（第2版）")
   ("jc-goo" "http://dictionary.goo.ne.jp/srch/jc/%s/m0u/"
    utf-8 nil "三省堂 デイリーコンサイス日中辞典")

   ;; ocn goo
   ("ej-ocn"
    "http://ocndictionary.goo.ne.jp/search.php?MT=%s&kind=ej&mode=0&kwassist=0"
    euc-jp nil "三省堂 EXCEED英和辞典")
   ("je-ocn"
    "http://ocndictionary.goo.ne.jp/search.php?MT=%s&kind=je&mode=0&kwassist=0"
    euc-jp nil "三省堂 EXCEED和英辞典")
   ("jj-ocn"
    "http://ocndictionary.goo.ne.jp/search.php?MT=%s&kind=jn&mode=0&kwassist=0"
    euc-jp nil "三省堂 大辞林第二版、デイリー新語辞典+α")
   ("all-ocn"
    "http://ocndictionary.goo.ne.jp/search.php?MT=%s&kind=all&mode=0&kwassist=0"
    euc-jp nil "すべての辞書")

   ;; gigadict
   ("JG-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicjg.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "日独辞典（ドイツ語）")
   ("GJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicgj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "独和辞典（ドイツ語）")
   ("JF-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicjf.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "日仏辞典（フランス語）")
   ("FJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicfj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "仏和辞典（フランス語）")
   ("PJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicpj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "葡和辞典（ポルトガル語）")
   ("JI-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicji.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "日伊辞典（イタリア語）")
   ("IJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicij.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "伊和辞典（イタリア語）")
   ("JS-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicjs.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "和西辞典（スペイン語）")
   ("SJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicsj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "西和辞典（スペイン語）")
   ("JK-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicjko.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "日韓辞典（韓国語）")
   ("KJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dickoj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "韓日辞典（韓国語）")
   ("JT-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicjt.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "和土辞典（トルコ語）")
   ("TJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dictj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "土和辞典（トルコ語）")
   ("JR-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicjr.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "和露辞典（ロシア語）")
   ("RJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicrj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "露和辞典（ロシア語）")
   ("JC-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicjc.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "日中辞典（中国語簡体）")
   ("CJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/diccj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "中日辞典（中国語簡体）")
   ("JN-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicjn.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "和蘭辞典（オランダ語）")
   ("NJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicnj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "蘭和辞典（オランダ語）")
   ("JH-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicjh.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "日本語-ヘブライ語辞典")
   ("HJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dichj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "ヘブライ語-日本語辞典")
   ("JAr-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicjar.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "日本語-アラビア語辞典")
   ("ArJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicarj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "アラビア語-日本語辞典")
   ("JE-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicje2.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "和英辞典（英語）")
   ("EJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicej.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "和英辞典（英語）")
   ("JFa-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicjfa.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "日本語-ペルシャ語辞典")
   ("FaJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicfaj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "ペルシャ語-日本語辞典")
   ("JPol-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicjpol.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "和波辞典（ポ-ランド語）")
   ("PolJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicpolj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "波和辞典（ポ-ランド語）")
   ("JU-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicju.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "日本語-ウクライナ語辞典")
   ("UJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicuj.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "ウクライナ語-日本語辞典")
   ("JIdn-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicJIdn.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "日本語-インドネシア語辞典")
   ("IndJ-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/dicIdnJ.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "インドネシア語-日本語辞典")
   ("Kanji-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/kanjidic.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "世界漢字字典（日本・中国・韓国の全漢字含む）")
   ("KKanji-gigadict"
    "http://cgi.geocities.jp/abelinternational/cgi/jkdic.cgi?mode=search&word=%s&page_max=50"
    utf-8 nil "日本語教育漢字熟語字典")

   ;; 北辞郎 中日
   ("cj-kitajiro" "http://www.ctrans.org/search.php?word=%s&opts=fw"
    utf-8 nil "北辞郎 中日辞書")
   ("jc-kitajiro" "http://www.ctrans.org/search.php?word=%s&opts=jp"
    utf-8 nil "北辞郎 日中辞書")
   ("pinyin-ctrans" "http://www.ctrans.org/pinconv.cgi"
    utf-8 "content=%s&submit=Pinconv&mode=pcv&chk=日本語" "北辞郎 ピンイン")

   ;; BitEx 中日
   ("cj-bitex"
    "http://bitex-cn.com/search_result.php?match=contains&keyword=%s&imageField.x=0&imageField.y=0&deal_type=cn2jp"
    utf-8 nil "BitEx 中日辞書")
   ("jc-bitex"
    "http://bitex-cn.com/search_result.php?match=contains&keyword=%s&imageField.x=0&imageField.y=0&deal_type=jp2cn"
    utf-8 nil "BitEx 日中辞書")

   ;; 敦煌辞海 中日
   ("cj-tonko-jikai"
    "http://www.onlinedic.com/search.php?searchtext=%s&lang=1" utf-8 nil
    "敦煌辞海 中日辞書")
   ("jc-tonko-jikai"
    "http://www.onlinedic.com/search.php?searchtext=%s&lang=0" utf-8 nil
    "敦煌辞海 日中辞書")
   ("cj-tonko-jikai-post" "http://www.onlinedic.com/search.php"
    utf-8 "searchtext=%s&lang=1" "敦煌辞海 中日辞書")
   ("jc-tonko-jikai-post" "http://www.onlinedic.com/search.php"
    utf-8 "searchtext=%s&lang=0" "敦煌辞海 日中辞書")

   ;; 楽訳中国語辞書
   ("jc-jcdic" "http://www.jcdic.com/search.php?searchtext=%s&lang=0"
    utf-8 nil "楽訳中国語辞書 日中")
   ("cj-jcdic" "http://www.jcdic.com/search.php?searchtext=%s&lang=1"
    utf-8 nil "楽訳中国語辞書 中日")
   ("pinyin-jcdic" "http://www.jcdic.com/chinese_convert/index.php"
    utf-8
    "codetxt=%s&remLen=800&cnbox=checked&twbox=checked&jpbox=checked&pybox=checked"
    "楽訳 ピンイン")

   ;; くじらはんど
   ("pinyin-kujirahand" "http://kujirahand.com/web-tools/pinyin.php"
    utf-8 "text=%s&t=jian&rb=0&pf=1&kana=0" "くじらはんど ピンイン")

   ;; MandarinSpot
   ("pinyin-mandarinspot"
    "http://mandarinspot.com/annotate?text=%s&spaces=1&phs=pinyin&show=both"
    utf-8 nil "MandarinSpot ピンイン")
   ("pinyin-mandarinspot-post" "http://mandarinspot.com/annotate"
    utf-8 "text=%s&spaces=1&phs=pinyin&show=both" "MandarinSpot ピンイン")

   ;; PinYin.JP
   ("pinyin-pinyin.jp" "http://pinyin.jp/hz2py.cgi?hz=%s&tone=f"
    utf-8 nil "PinYin.JP ピンイン")

   ;; hjenglish 中日
   ("cj-hjenglish" "http://dict.hjenglish.com/jp/w/%s&type=cj" utf-8 nil
    "获得小D英日双核海量桌面词典 中日")
   ("jc-hjenglish" "http://dict.hjenglish.com/jp/w/%s&type=jc" utf-8 nil
    "获得小D英日双核海量桌面词典 日中")

   ;; Wiktionary
   ("jj-wiktionary" "http://ja.wiktionary.org/wiki/%s" utf-8 nil
    "ウィクショナリー日本語版(Wiktionary)")
   ("kanji-wiktionary" "http://ja.wiktionary.org/wiki/%s" utf-8 nil)
   ("ee-wiktionary" "http://en.wiktionary.org/wiki/%s" utf-8 nil)

   ;; 書虫 pinyin
   ("pinyin-frelax" "http://www.frelax.com/cgi-local/pinyin/hz2py.cgi"
    utf-8 "hanzi=%s&mark=3&jthz=3" "書虫 ピンイン")

   ;; 成蹊大学 中国語音声教育データベースシステム 要ユーザ登録
   ;; http://chinese.jim.seikei.ac.jp/chinese/LoginInit.do?ap=&zh=
   ("pinyin-seikei"
    "http://chinese.jim.seikei.ac.jp/chinese/SearchList_chiInit.do" utf-8
    "Act=1&txtSearch=%s&searchButton=検索&action_cl=1" "成蹊大学 ピンイン")

   ;; pinyin chinese1
   ("pinyin-chinese1"
    "http://www.chinese1.jp/pinyin/gb2312/jp.asp?wenzi=%s" gb2312 nil
    "中文広場 ピンイン")

   ;; pinyin cazoo
   ("pinyin-cazoo" "http://www.cazoo.jp/cgi-bin/pinyin/index.html?hanzi=%s"
    utf-8 nil "Cazoo ピンイン")

   ;; dokochina pinyin
   ("pinyin-dokochina" "http://dokochina.com/simplified.php"
    utf-8 "text1=%s&kbn1=1&chk1=0" "どんと来い、中国語 ピンイン")

   ;; 三修社 独和辞典
   ("gj-sanshusha" "http://www5.mediagalaxy.co.jp/CGI/sanshushadj/search.cgi"
    shift_jis "key_word=%s&cmd=list" "三修社 独和辞典")
   ("gj-sanshusha-get"
    "http://www5.mediagalaxy.co.jp/CGI/sanshushadj/search.cgikey_word=%s&cmd=list"
    shift_jis nil "三修社 独和辞典")

   ;; spellcheck.net spell checker
   ("spell-spellcheck" "http://www.spellcheck.net/cgi-bin/spell.exe" nil
    "action=CHECKPG&string=%s" "英語スペルチェッカー")
   ("spell-spellcheck-get"
    "http://www.spellcheck.net/cgi-bin/spell.exe?action=CHECKPG&string=%s"
    nil nil "英語スペルチェッカー")

   ;; 通信用語の基礎知識
   ("tsuushinyougo-wdic.org" "http://www.wdic.org/search?word=%s"
    utf-8 nil "通信用語の基礎知識")

   ;; チュウ太のweb辞書
   ("jj-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=ja"
    utf-8 nil "ED電子化辞書")
   ("je-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=en"
    utf-8 nil "EDR日英対訳辞書")
   ("jc-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=zh"
    utf-8 nil "EDR")
   ("jaaa-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=aa"
    utf-8 nil "Reading Tutor Afar")
   ("jaar-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=ar"
    utf-8 nil "Reading Tutor Arabic")
   ("jabg-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=bg"
    utf-8 nil "Reading Tutor Bulgarian")
   ("jacs-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=cs"
    utf-8 nil "Reading Tutor Czech")
   ("jafi-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=fi"
    utf-8 nil "Reading Tutor Finnish")
   ("jafr-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=fr"
    utf-8 nil "Reading Tutor French")
   ("jade-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=de"
    utf-8 nil "Reading Tutor German")
   ("jahu-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=hu"
    utf-8 nil "Reading Tutor Hungarian")
   ("jait-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=it"
    utf-8 nil "Reading Tutor Italian")
   ("jakir-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=kir"
    utf-8 nil "Reading Tutor Kirghiz")
   ("jako-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=ko"
    utf-8 nil "Reading Tutor Korean")
   ("jams-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=ms"
    utf-8 nil "Reading Tutor Malay")
   ("jamao-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=mao"
    utf-8 nil "Reading Tutor Maori")
   ("jamr-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=mr"
    utf-8 nil "Reading Tutor Marathi")
   ("janah-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=nah"
    utf-8 nil "Reading Tutor Nahuatl")
   ("japt-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=pt"
    utf-8 nil "Reading Tutor Portuguese")
   ("jaro-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=ro"
    utf-8 nil "Reading Tutor Romanian")
   ("jaru-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=ru"
    utf-8 nil "Reading Tutor Russian")
   ("jask-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=sk"
    utf-8 nil "Reading Tutor Slovak")
   ("jasl-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=sl"
    utf-8 nil "Reading Tutor Slovenian")
   ("jaes-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=es"
    utf-8 nil "Reading Tutor Spanish")
   ("jatl-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=tl"
    utf-8 nil "Reading Tutor Tagalog")
   ("jath-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=th"
    utf-8 nil "Reading Tutor Thai")
   ("jatr-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=tr"
    utf-8 nil "Reading Tutor Turkish")
   ("javi-chuuta" "http://marmot.chuta.jp/edit1/Dic.aspx?Search=%s&lang=vi"
    utf-8 nil "Reading Tutor Vietnamese")
   ("jj-chuuta2" "http://marmot.chuta.jp/jtool/jtool.cgi"
    utf-8 "SENTENCE=%s&DIC=Dictionary&lang=en&lang=ja&lang=zh"
    "ED電子化辞書")

   ;; WordReference.com
   ("enes-wordreference" "http://www.wordreference.com/enes/%s"
    utf-8 nil "English-Spanish")
   ("enfr-wordreference" "http://www.wordreference.com/enfr/%s"
    utf-8 nil "English-French")
   ("enit-wordreference" "http://www.wordreference.com/enit/%s"
    utf-8 nil "English-Italian")
   ("ende-wordreference" "http://www.wordreference.com/ende/%s"
    utf-8 nil "English-German")
   ("enru-wordreference" "http://www.wordreference.com/enru/%s"
    utf-8 nil "English-Russian")
   ("enpt-wordreference" "http://www.wordreference.com/enpt/%s"
    utf-8 nil "English-Portuguese")
   ("enpl-wordreference" "http://www.wordreference.com/enpl/%s"
    utf-8 nil "English-Polish")
   ("enro-wordreference" "http://www.wordreference.com/enro/%s"
    utf-8 nil "English-Romanian")
   ("encz-wordreference" "http://www.wordreference.com/encz/%s"
    utf-8 nil "English-Czech")
   ("engr-wordreference" "http://www.wordreference.com/engr/%s"
    utf-8 nil "English-Greek")
   ("entr-wordreference" "http://www.wordreference.com/entr/%s"
    utf-8 nil "English-Turkish")
   ("enzh-wordreference" "http://www.wordreference.com/enzh/%s"
    utf-8 nil "English-Chinese")
   ("zhen-wordreference" "http://www.wordreference.com/zhen/%s"
    utf-8 nil "Chinese-English")
   ("enja-wordreference" "http://www.wordreference.com/enja/%s"
    utf-8 nil "English-Japanese")
   ("jaen-wordreference" "http://www.wordreference.com/jaen/%s"
    utf-8 nil "Japanese-English")
   ("enko-wordreference" "http://www.wordreference.com/enko/%s"
    utf-8 nil "English-Korean")
   ("koen-wordreference" "http://www.wordreference.com/koen/%s"
    utf-8 nil "Korean-English")
   ("enar-wordreference" "http://www.wordreference.com/enar/%s"
    utf-8 nil "English-Arabic")
   ("enen-wordreference" "http://www.wordreference.com/definition/%s"
    utf-8 nil "English definition")
   ("esen-wordreference" "http://www.wordreference.com/esen/%s"
    utf-8 nil "Spanish-English")
   ("fren-wordreference" "http://www.wordreference.com/fren/%s"
    utf-8 nil "French-English")
   ("iten-wordreference" "http://www.wordreference.com/iten/%s"
    utf-8 nil "Italian-English")
   ("deen-wordreference" "http://www.wordreference.com/deen/%s"
    utf-8 nil "German-English")
   ("ruen-wordreference" "http://www.wordreference.com/ruen/%s"
    utf-8 nil "Russian-English")
   ("esfr-wordreference" "http://www.wordreference.com/esfr/%s"
    utf-8 nil "Spanish-French")
   ("espt-wordreference" "http://www.wordreference.com/espt/%s"
    utf-8 nil "Spanish-Portuguese")
   ("eses-wordreference" "http://www.wordreference.com/definicion/%s"
    utf-8 nil "Spanish definition")
   ("essin-wordreference" "http://www.wordreference.com/sinonimos/%s"
    utf-8 nil "Spanish synonyms")
   ("fres-wordreference" "http://www.wordreference.com/fres/%s"
    utf-8 nil "French-Spanish")
   ("ptes-wordreference" "http://www.wordreference.com/ptes/%s"
    utf-8 nil "Portuguese-Spanish")

   ;; JMdict (Jim Breen's WWWJDIC)
   ("je-jmdict" "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=1" "Jpn-Eng General (EDICT) Jim Breen's WWWJDIC")
   ("je-jmdict-JapaneseNames"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=2" "Japanese Names (ENAMDICT) Jim Breen's WWWJDIC")
   ("je-jmdict-Computing/Telecomms"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=3" "Computing/Telecomms Jim Breen's WWWJDIC")
   ("je-jmdict-LifeSciences/Bio-Med"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=4" "Life Sciences/Bio-Med Jim Breen's WWWJDIC")
   ("je-jmdict-LegalTerms"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=5" "Legal Terms Jim Breen's WWWJDIC")
   ("je-jmdict-Finance/Marketing"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=6" "Finance/Marketing Jim Breen's WWWJDIC")
   ("je-jmdict-Buddhism"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=7" "Buddhism Jim Breen's WWWJDIC")
   ("je-jmdict-Miscellaneous"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=8" "Miscellaneous Jim Breen's WWWJDIC")
   ("je-jmdict-SpecialText-glossing"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=9" "Special Text-glossing Jim Breen's WWWJDIC")
   ("je-jmdict-Engineering/Science"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=A" "Engineering/Science Jim Breen's WWWJDIC")
   ("je-jmdict-Linguistics"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=B" "Linguistics Jim Breen's WWWJDIC")
   ("je-jmdict-River&WaterSystems"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=C" "River & Water Systems Jim Breen's WWWJDIC")
   ("je-jmdict-Automobile Industry"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=D" "Automobile Industry Jim Breen's WWWJDIC")
   ("je-jmdict-Japanese Wordnet"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=E" "Japanese Wordnet Jim Breen's WWWJDIC")
   ("je-jmdict-Work-in-progressFile"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=F" "Work-in-progress File Jim Breen's WWWJDIC")
   ("je-jmdict-Japanese-German"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=G" "Japanese-German (WaDoku) Jim Breen's WWWJDIC")
   ("je-jmdict-Japanese-French"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=H" "Japanese-French Jim Breen's WWWJDIC")
   ("je-jmdict-Japanese-Russian"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=I" "Japanese-Russian Jim Breen's WWWJDIC")
   ("je-jmdict-Japanese-Swedish"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=J" "Japanese-Swedish Jim Breen's WWWJDIC")
   ("je-jmdict-Japanese-Hungarian"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=K" "Japanese-Hungarian Jim Breen's WWWJDIC")
   ("je-jmdict-Japanese-Spanish"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=L" "Japanese-Spanish Jim Breen's WWWJDIC")
   ("je-jmdict-Japanese-Dutch"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=M" "Japanese-Dutch Jim Breen's WWWJDIC")
   ("je-jmdict-Japanese-Slovenian"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=N" "Japanese-Slovenian Jim Breen's WWWJDIC")
   ("je-jmdict-Untranslated"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=O" "Untranslated Jim Breen's WWWJDIC")
   ("je-jmdict-Combined Jpn-Eng"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=P" "Combined Jpn-Eng Jim Breen's WWWJDIC")
   ("je-jmdict-Expanded Text-glossing"
    "http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?1E"
    euc-jp "dsrchkey=%s&dicsel=Q" "Expanded Text-glossing Jim Breen's WWWJDIC")

   ;; JapaneseClass.jp
   ("je-japaneseclass"
    "http://japaneseclass.jp/tools/dictionary/%s" utf-8 nil
    "Japanese-English in English")
   ("ej-japaneseclass"
    "http://japaneseclass.jp/tools/dictionary/%s" utf-8 nil
    "English-Japanese in English")

   ;; NAVER
   ("kj-naver" "http://krdic.naver.jp/search/all/%s/"
    utf-8 nil "NAVER 韓日辞書")
   ("jk-naver" "http://krdic.naver.jp/search/all/%s/"
    utf-8 nil "NAVER 日韓辞書")
   ("cj-naver" "http://cndic.naver.jp/srch/all/1/%s"
    utf-8 nil "NAVER 中日辞書")
   ("jc-naver" "http://cndic.naver.jp/srch/all/1/%s"
    utf-8 nil "NAVER 日中辞書")
   ("ej-naver" "http://endic.naver.jp/srch/all/N/%s"
    utf-8 nil "NAVER 英和辞書")
   ("je-naver" "http://endic.naver.jp/srch/all/N/%s"
    utf-8 nil "NAVER 和英辞書")

   ;;
   ;; translators
   ;;

   ;; yahoo translator
   ("tr-ej-yahoo" "http://honyaku.yahoo.co.jp/transtext" utf-8
    "both=TH&text=%s&clearFlg=1&eid=CR-EJ" "Cross Language"
    dic-lookup-w3m-suitable-engine-pattern)
   ("tr-je-yahoo" "http://honyaku.yahoo.co.jp/transtext" utf-8
    "both=TH&text=%s&clearFlg=1&eid=CR-JE" "Cross Language")
   ("tr-cj-yahoo" "http://honyaku.yahoo.co.jp/transtext" utf-8
    "both=TH&text=%s&clearFlg=1&eid=CR-CJ" "Cross Language")
   ("tr-jc-yahoo" "http://honyaku.yahoo.co.jp/transtext" utf-8
    "both=TH&text=%s&clearFlg=1&eid=CR-JC-CN" "Cross Language")
   ("tr-kj-yahoo" "http://honyaku.yahoo.co.jp/transtext" utf-8
    "both=TH&text=%s&clearFlg=1&eid=CR-KJ" "Changshin Soft (CSLI)")
   ("tr-jk-yahoo" "http://honyaku.yahoo.co.jp/transtext" utf-8
    "both=TH&text=%s&clearFlg=1&eid=CR-JK" "Changshin Soft (CSLI)")

   ("tr-enja-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=en&oeid=ja&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-zhja-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=zh&oeid=ja&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-koja-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=ko&oeid=ja&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-frja-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=fr&oeid=ja&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-deja-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=de&oeid=ja&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-esja-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=es&oeid=ja&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-ptja-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=pt&oeid=ja&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-itja-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=it&oeid=ja&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-jaen-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=ja&oeid=en&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-jazh-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=ja&oeid=zh&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-jako-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=ja&oeid=ko&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-jafr-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=ja&oeid=fr&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-jade-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=ja&oeid=de&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-jaes-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=ja&oeid=es&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-japi-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=ja&oeid=pt&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")
   ("tr-jait-url-yahoo"
    "http://honyaku.yahoofs.jp/url_result?ieid=ja&oeid=it&both=T&setting=for\%%3D0&url=%s"
    utf-8 nil "")

   ;; excite translator
   ,@(dic-lookup-w3m-search-engine-postget
      '(("tr-enja-excite" "http://www.excite.co.jp/world/english/"
	 shift_jis "wb_lp=ENJA&before=%s" "英日 BizLingo (Accela Technology)"
	 dic-lookup-w3m-suitable-engine-pattern)
	("tr-jaen-excite" "http://www.excite.co.jp/world/english/"
	 shift_jis "wb_lp=JAEN&before=%s" "日英 BizLingo (Accela Technology)")
	("tr-chja-excite" "http://www.excite.co.jp/world/chinese/"
	 utf-8 "wb_lp=CHJA&big5=no&before=%s" "中日 Kodensha")
	("tr-jach-excite" "http://www.excite.co.jp/world/chinese/"
	 utf-8 "wb_lp=JACH&big5=no&before=%s" "日中 Kodensha")
	("tr-twja-excite" "http://www.excite.co.jp/world/chinese/"
	 utf-8 "wb_lp=CHJA&big5=yes&before=%s" "台中 Kodensha")
	("tr-jatw-excite" "http://www.excite.co.jp/world/chinese/"
	 utf-8 "wb_lp=JACH&big5=yes&before=%s" "中台 Kodensha")
	("tr-koja-excite" "http://www.excite.co.jp/world/korean/"
	 utf-8 "wb_lp=KOJA&before=%s" "韓日 Amikai")
	("tr-jako-excite" "http://www.excite.co.jp/world/korean/"
	 utf-8 "wb_lp=JAKO&before=%s" "日韓 Amikai")
	("tr-frja-excite" "http://www.excite.co.jp/world/french/"
	 utf-8 "wb_lp=FRJA&before=%s" "仏日")
	("tr-jafr-excite" "http://www.excite.co.jp/world/french/"
	 utf-8 "wb_lp=JAFR&before=%s" "日仏")
	("tr-fren-excite" "http://www.excite.co.jp/world/french/"
	 utf-8 "wb_lp=FREN&before=%s" "仏英")
	("tr-enfr-excite" "http://www.excite.co.jp/world/french/"
	 utf-8 "wb_lp=ENFR&before=%s" "英仏")
	("tr-deja-excite" "http://www.excite.co.jp/world/german/"
	 utf-8 "wb_lp=DEJA&before=%s" "独日")
	("tr-jade-excite" "http://www.excite.co.jp/world/german/"
	 utf-8 "wb_lp=JADE&before=%s" "日独")
	("tr-deen-excite" "http://www.excite.co.jp/world/german/"
	 utf-8 "wb_lp=DEEN&before=%s" "独英")
	("tr-ende-excite" "http://www.excite.co.jp/world/german/"
	 utf-8 "wb_lp=ENDE&before=%s" "英独")
	("tr-itja-excite" "http://www.excite.co.jp/world/italian/"
	 utf-8 "wb_lp=ITJA&before=%s" "伊日")
	("tr-jait-excite" "http://www.excite.co.jp/world/italian/"
	 utf-8 "wb_lp=JAIT&before=%s" "日伊")
	("tr-iten-excite" "http://www.excite.co.jp/world/italian/"
	 utf-8 "wb_lp=ITEN&before=%s" "伊英")
	("tr-enit-excite" "http://www.excite.co.jp/world/italian/"
	 utf-8 "wb_lp=ENIT&before=%s" "英伊")
	("tr-esja-excite" "http://www.excite.co.jp/world/spanish/"
	 utf-8 "wb_lp=ESJA&before=%s" "西日")
	("tr-jaes-excite" "http://www.excite.co.jp/world/spanish/"
	 utf-8 "wb_lp=JAES&before=%s" "日西")
	("tr-esen-excite" "http://www.excite.co.jp/world/spanish/"
	 utf-8 "wb_lp=ESEN&before=%s" "西英")
	("tr-enes-excite" "http://www.excite.co.jp/world/spanish/"
	 utf-8 "wb_lp=ENES&before=%s" "英西")
	("tr-ptja-excite" "http://www.excite.co.jp/world/portuguese/"
	 utf-8 "wb_lp=PTJA&before=%s" "葡日")
	("tr-japt-excite" "http://www.excite.co.jp/world/portuguese/"
	 utf-8 "wb_lp=JAPT&before=%s" "日葡")
	("tr-pten-excite" "http://www.excite.co.jp/world/portuguese/"
	 utf-8 "wb_lp=PTEN&before=%s" "葡英")
	("tr-enpt-excite" "http://www.excite.co.jp/world/portuguese/"
	 utf-8 "wb_lp=ENPT&before=%s" "英葡")
	))

   ,@(dic-lookup-w3m-search-engine-postget
      '(("tr-enja-url-excite" "http://www.excite.co.jp/world/english/web/"
	 utf-8 "wb_lp=ENJA&wb_url=%s" "英日")
	("tr-jaen-url-excite" "http://www.excite.co.jp/world/english/web/"
	 utf-8 "wb_lp=JAEN&wb_url=%s" "日英")
	("tr-chja-url-excite" "http://www.excite.co.jp/world/chinese/web/"
	 utf-8 "wb_lp=CHJA&big5=no&wb_url=%s" "中日")
	("tr-jach-url-excite" "http://www.excite.co.jp/world/chinese/web/"
	 utf-8 "wb_lp=JACH&big5=no&wb_url=%s" "日中")
	("tr-twja-url-excite" "http://www.excite.co.jp/world/chinese/web/"
	 utf-8 "wb_lp=CHJA&big5=yes&wb_url=%s" "台日")
	("tr-jatw-url-excite" "http://www.excite.co.jp/world/chinese/web/"
	 utf-8 "wb_lp=JACH&big5=yes&wb_url=%s" "日台")
	("tr-koja-url-excite" "http://www.excite.co.jp/world/korean/web/"
	 utf-8 "wb_lp=KOJA&wb_url=%s" "韓日")
	("tr-jako-url-excite" "http://www.excite.co.jp/world/korean/web/"
	 utf-8 "wb_lp=JAKO&wb_url=%s" "日韓")
	("tr-frja-url-excite" "http://www.excite.co.jp/world/french/web/"
	 utf-8 "wb_lp=FRJA&wb_url=%s" "仏日")
	("tr-jafr-url-excite" "http://www.excite.co.jp/world/french/web/"
	 utf-8 "wb_lp=JAFR&wb_url=%s" "日仏")
	("tr-fren-url-excite" "http://www.excite.co.jp/world/french/web/"
	 utf-8 "wb_lp=FREN&wb_url=%s" "仏英")
	("tr-enfr-url-excite" "http://www.excite.co.jp/world/french/web/"
	 utf-8 "wb_lp=ENFR&wb_url=%s" "英仏")
	("tr-deja-url-excite" "http://www.excite.co.jp/world/german/web/"
	 utf-8 "wb_lp=DEJA&wb_url=%s" "独日")
	("tr-jade-url-excite" "http://www.excite.co.jp/world/german/web/"
	 utf-8 "wb_lp=JADE&wb_url=%s" "日独")
	("tr-deen-url-excite" "http://www.excite.co.jp/world/german/web/"
	 utf-8 "wb_lp=DEEN&wb_url=%s" "独英")
	("tr-ende-url-excite" "http://www.excite.co.jp/world/german/web/"
	 utf-8 "wb_lp=ENDE&wb_url=%s" "英独")
	("tr-itja-url-excite" "http://www.excite.co.jp/world/italian/web/"
	 utf-8 "wb_lp=ITJA&wb_url=%s" "伊日")
	("tr-jait-url-excite" "http://www.excite.co.jp/world/italian/web/"
	 utf-8 "wb_lp=JAIT&wb_url=%s" "日伊")
	("tr-iten-url-excite" "http://www.excite.co.jp/world/italian/web/"
	 utf-8 "wb_lp=ITEN&wb_url=%s" "伊英")
	("tr-enit-url-excite" "http://www.excite.co.jp/world/italian/web/"
	 utf-8 "wb_lp=ENIT&wb_url=%s" "英伊")
	("tr-esja-url-excite" "http://www.excite.co.jp/world/spanish/web/"
	 utf-8 "wb_lp=ESJA&wb_url=%s" "西日")
	("tr-jaes-url-excite" "http://www.excite.co.jp/world/spanish/web/"
	 utf-8 "wb_lp=JAES&wb_url=%s" "日西")
	("tr-esen-url-excite" "http://www.excite.co.jp/world/spanish/web/"
	 utf-8 "wb_lp=ESEN&wb_url=%s" "西英")
	("tr-enes-url-excite" "http://www.excite.co.jp/world/spanish/web/"
	 utf-8 "wb_lp=ENES&wb_url=%s" "英西")
	("tr-ptja-url-excite" "http://www.excite.co.jp/world/portuguese/web/"
	 utf-8 "wb_lp=PTJA&wb_url=%s" "葡日")
	("tr-japt-url-excite" "http://www.excite.co.jp/world/portuguese/web/"
	 utf-8 "wb_lp=JAPT&wb_url=%s" "日葡")
	("tr-pten-url-excite" "http://www.excite.co.jp/world/portuguese/web/"
	 utf-8 "wb_lp=PTEN&wb_url=%s" "葡英")
	("tr-enpt-url-excite" "http://www.excite.co.jp/world/portuguese/web/"
	 utf-8 "wb_lp=ENPT&wb_url=%s" "英葡")
	))

   ;; yahoo.com translator
   ;; Accept-Charsetにutf-8が必要
   ;; (setq w3m-command-arguments
   ;;       '("-header" "Accept-Charset: ISO-2022-JP, EUC-JP, Shift-JIS, UTF-8;q=0.8, *;q=0.1"))
   ,@(dic-lookup-w3m-search-engine-postget
      '(("tr-ej-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_ja&btnTrTxt=Translate"
	 "babelfish" dic-lookup-w3m-suitable-engine-pattern)
	("tr-je-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=ja_en&btnTrTxt=Translate"
	 "babelfish")
	("tr-ennl-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_nl&btnTrTxt=Translate"
	 "babelfish")
	("tr-nlen-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=nl_en&btnTrTxt=Translate"
	 "babelfish")
	("tr-enfr-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_fr&btnTrTxt=Translate"
	 "babelfish")
	("tr-fren-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_en&btnTrTxt=Translate"
	 "babelfish")
	("tr-ende-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_de&btnTrTxt=Translate"
	 "babelfish")
	("tr-deen-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=de_en&btnTrTxt=Translate"
	 "babelfish")
	("tr-enel-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_el&btnTrTxt=Translate"
	 "babelfish")
	("tr-elen-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=el_en&btnTrTxt=Translate"
	 "babelfish")
	("tr-enit-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_it&btnTrTxt=Translate"
	 "babelfish")
	("tr-iten-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=it_en&btnTrTxt=Translate"
	 "babelfish")
	("tr-enko-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_ko&btnTrTxt=Translate"
	 "babelfish")
	("tr-koen-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=ko_en&btnTrTxt=Translate"
	 "babelfish")
	("tr-enpt-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_pt&btnTrTxt=Translate"
	 "babelfish")
	("tr-pten-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=pt_en&btnTrTxt=Translate"
	 "babelfish")
	("tr-enru-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_ru&btnTrTxt=Translate"
	 "babelfish")
	("tr-ruen-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=ru_en&btnTrTxt=Translate"
	 "babelfish")
	("tr-enes-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_es&btnTrTxt=Translate"
	 "babelfish")
	("tr-esen-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=es_en&btnTrTxt=Translate"
	 "babelfish")
	("tr-nlfr-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=nl_fr&btnTrTxt=Translate"
	 "babelfish")
	("tr-frnl-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_nl&btnTrTxt=Translate"
	 "babelfish")
	("tr-frde-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_de&btnTrTxt=Translate"
	 "babelfish")
	("tr-defr-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=de_fr&btnTrTxt=Translate"
	 "babelfish")
	("tr-frel-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_el&btnTrTxt=Translate"
	 "babelfish")
	("tr-elfr-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=el_fr&btnTrTxt=Translate"
	 "babelfish")
	("tr-frit-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_it&btnTrTxt=Translate"
	 "babelfish")
	("tr-itfr-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=it_fr&btnTrTxt=Translate"
	 "babelfish")
	("tr-frpt-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_pt&btnTrTxt=Translate"
	 "babelfish")
	("tr-ptfr-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=pt_fr&btnTrTxt=Translate"
	 "babelfish")
	("tr-fres-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=fr_es&btnTrTxt=Translate"
	 "babelfish")
	("tr-esfr-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=es_fr&btnTrTxt=Translate"
	 "babelfish")
	("tr-ench-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_zh&btnTrTxt=Translate"
	 "babelfish")
	("tr-chen-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=zh_en&btnTrTxt=Translate"
	 "babelfish")
	("tr-entw-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=en_zt&btnTrTxt=Translate"
	 "babelfish")
	("tr-twen-yahoo.com" "http://babelfish.yahoo.com/translate_txt" utf-8
	 "ei=UTF-8&doit=done&intl=1&tt=urltext&trtext=%s&lp=zt_en&btnTrTxt=Translate"
	 "babelfish")
	))

   ;; freetranslation
   ,@(dic-lookup-w3m-search-engine-postget
      '(("tr-ej-freetranslation" "http://tets9.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Japanese&srctext=%s"
	 nil dic-lookup-w3m-suitable-engine-pattern)
	("tr-je-freetranslation" "http://tets9.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=Japanese/English&srctext=%s")
	("tr-enes-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Spanish&srctext=%s")
	("tr-esen-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=Spanish/English&srctext=%s")
	("tr-enfr-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/French&srctext=%s")
	("tr-fren-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=French/English&srctext=%s")
	("tr-ende-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/German&srctext=%s")
	("tr-deen-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=German/English&srctext=%s")
	("tr-enit-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Italian&srctext=%s")
	("tr-iten-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=Italian/English&srctext=%s")
	("tr-ennl-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Dutch&srctext=%s")
	("tr-nlen-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=Dutch/English&srctext=%s")
	("tr-enpt-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Portuguese&srctext=%s")
	("tr-pten-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=Portuguese/English&srctext=%s")
	("tr-enru-freetranslation" "http://ets6.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Russian&srctext=%s")
	("tr-ruen-freetranslation" "http://ets6.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=Russian/English&srctext=%s")
	("tr-ench-freetranslation" "http://ets6.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/SimplifiedChinese&srctext=%s")
	("tr-entw-freetranslation" "http://ets6.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/TraditionalChinese&srctext=%s")
	("tr-enno-freetranslation" "http://ets.freetranslation.com" utf-8
	 "sequence=core&mode=html&charset=UTF-8&template=results_en-us.htm&language=English/Norwegian&srctext=%s")
	))

   ;; ;; ocn translator サービス終了 2012年10月11日
   ;; ("tr-ej-ocn" "http://cgi01.ocn.ne.jp/cgi-bin/translation/index.cgi"
   ;;  utf-8 "langpair=enja&sourceText=%s" "Kodensha")
   ;; ("tr-je-ocn" "http://cgi01.ocn.ne.jp/cgi-bin/translation/index.cgi"
   ;;  utf-8 "langpair=jaen&sourceText=%s" "Kodensha")
   ;; ("tr-kj-ocn" "http://cgi01.ocn.ne.jp/cgi-bin/translation/index.cgi"
   ;;  utf-8 "langpair=koja&sourceText=%s" "Kodensha")
   ;; ("tr-jk-ocn" "http://cgi01.ocn.ne.jp/cgi-bin/translation/index.cgi"
   ;;  utf-8 "langpair=jako&sourceText=%s" "Kodensha")
   ;; ("tr-cj-ocn" "http://cgi01.ocn.ne.jp/cgi-bin/translation/index.cgi"
   ;;  utf-8 "langpair=zhja&sourceText=%s" "Kodensha")
   ;; ("tr-jc-ocn" "http://cgi01.ocn.ne.jp/cgi-bin/translation/index.cgi"
   ;;  utf-8 "langpair=jazh&sourceText=%s" "Kodensha")
   ;; ;; ocn web page translaor
   ;; ("tr-ej-url-ocn"
   ;;  "http://translate.ocn.ne.jp/LUCOCN/c3/hm_ex.cgi?SURL=%s&XTYPE=1&SEARCH=T&SLANG=en&TLANG=ja"
   ;;  utf-8 nil "Kodensha")
   ;; ("tr-je-url-ocn"
   ;;  "http://translate.ocn.ne.jp/LUCOCN/c3/hm_ex.cgi?SURL=%s&XTYPE=1&SEARCH=T&SLANG=ja&TLANG=en"
   ;;  utf-8 nil "Kodensha")
   ;; ("tr-kj-url-ocn"
   ;;  "http://translate.ocn.ne.jp/LUCOCN/c3/hm_ex.cgi?SURL=%s&XTYPE=1&SEARCH=T&SLANG=ko&TLANG=ja"
   ;;  utf-8 nil "Kodensha")
   ;; ("tr-jk-url-ocn"
   ;;  "http://translate.ocn.ne.jp/LUCOCN/c3/hm_ex.cgi?SURL=%s&XTYPE=1&SEARCH=T&SLANG=ja&TLANG=ko"
   ;;  utf-8 nil "Kodensha")
   ;; ("tr-cj-url-ocn"
   ;;  "http://translate.ocn.ne.jp/LUCOCN/c3/hm_ex.cgi?SURL=%s&XTYPE=1&SEARCH=T&SLANG=zh&TLANG=ja"
   ;;  utf-8 nil "Kodensha")
   ;; ("tr-jc-url-ocn"
   ;;  "http://translate.ocn.ne.jp/LUCOCN/c3/hm_ex.cgi?SURL=%s&XTYPE=1&SEARCH=T&SLANG=ja&TLANG=zh"
   ;;  utf-8 nil "Kodensha")

   ;; livedoor translator
   ("tr-zhja-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=zh&translateParams.tlang=ja&translateParams.originalText=%s")
   ("tr-koja-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=ko&translateParams.tlang=ja&translateParams.originalText=%s")
   ("tr-ptja-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=pt&translateParams.tlang=ja&translateParams.originalText=%s")
   ("tr-esja-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=es&translateParams.tlang=ja&translateParams.originalText=%s")
   ("tr-itja-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=it&translateParams.tlang=ja&translateParams.originalText=%s")
   ("tr-frja-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=fr&translateParams.tlang=ja&translateParams.originalText=%s")
   ("tr-deja-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=de&translateParams.tlang=ja&translateParams.originalText=%s")
   ("tr-jazh-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=ja&translateParams.tlang=zh&translateParams.originalText=%s")
   ("tr-jako-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=ja&translateParams.tlang=ko&translateParams.originalText=%s")
   ("tr-japt-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=ja&translateParams.tlang=pt&translateParams.originalText=%s")
   ("tr-jaes-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=ja&translateParams.tlang=es&translateParams.originalText=%s")
   ("tr-jait-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=ja&translateParams.tlang=it&translateParams.originalText=%s")
   ("tr-jafr-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=ja&translateParams.tlang=fr&translateParams.originalText=%s")
   ("tr-jade-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=ja&translateParams.tlang=de&translateParams.originalText=%s")
   ("tr-je-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=ja&translateParams.tlang=en&translateParams.originalText=%s")
   ("tr-pten-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=pt&translateParams.tlang=en&translateParams.originalText=%s")
   ("tr-esen-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=es&translateParams.tlang=en&translateParams.originalText=%s")
   ("tr-iten-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=it&translateParams.tlang=en&translateParams.originalText=%s")
   ("tr-fren-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=fr&translateParams.tlang=en&translateParams.originalText=%s")
   ("tr-deen-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=de&translateParams.tlang=en&translateParams.originalText=%s")
   ("tr-enpt-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=en&translateParams.tlang=pt&translateParams.originalText=%s")
   ("tr-enes-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=en&translateParams.tlang=es&translateParams.originalText=%s")
   ("tr-enit-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=en&translateParams.tlang=it&translateParams.originalText=%s")
   ("tr-enfr-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=en&translateParams.tlang=fr&translateParams.originalText=%s")
   ("tr-ende-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=en&translateParams.tlang=de&translateParams.originalText=%s")
   ("tr-ej-livedoor" "http://livedoor-translate.naver.jp/text/" utf-8
    "translateParams.slang=en&translateParams.tlang=ja&translateParams.originalText=%s")

   ;; livedoor web page translator
   ("tr-ej-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/enja/%s" utf-8)
   ("tr-ende-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/ende/%s" utf-8)
   ("tr-enfr-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/enfr/%s" utf-8)
   ("tr-enit-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/enit/%s" utf-8)
   ("tr-enes-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/enes/%s" utf-8)
   ("tr-enpt-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/enpt/%s" utf-8)
   ("tr-deen-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/deen/%s" utf-8)
   ("tr-fren-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/fren/%s" utf-8)
   ("tr-iten-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/iten/%s" utf-8)
   ("tr-esen-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/esen/%s" utf-8)
   ("tr-pten-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/pten/%s" utf-8)
   ("tr-je-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/jaen/%s" utf-8)
   ("tr-jade-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/jade/%s" utf-8)
   ("tr-jafr-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/jafr/%s" utf-8)
   ("tr-jait-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/jait/%s" utf-8)
   ("tr-jaes-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/jaes/%s" utf-8)
   ("tr-japt-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/japt/%s" utf-8)
   ("tr-jako-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/jako/%s" utf-8)
   ("tr-jazh-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/jazh/%s" utf-8)
   ("tr-deja-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/deja/%s" utf-8)
   ("tr-frja-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/frja/%s" utf-8)
   ("tr-itja-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/itja/%s" utf-8)
   ("tr-esja-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/esja/%s" utf-8)
   ("tr-ptja-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/ptja/%s" utf-8)
   ("tr-koja-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/koja/%s" utf-8)
   ("tr-zhja-url-livedoor"
    "http://livedoor-translate.naver.jp/site/translate/zhja/%s" utf-8)

   ;; fresheye translator
   ,@(dic-lookup-w3m-search-engine-postget
      '(("tr-ej-fresheye"
	 "http://mt.fresheye.com/ft_result.cgi"
	 utf-8 "gen_text=%s&e=EJ" "Toshiba (Amikai)")
	("tr-je-fresheye"
	 "http://mt.fresheye.com/ft_result.cgi"
	 utf-8 "gen_text=%s&e=JE" "Toshiba (Amikai)")
	("tr-jc-fresheye"
	 "http://mt.fresheye.com/ft_cjresult.cgi"
	 utf-8 "gen_text=%s&charset=gb2312&cjjc=jc" "Toshiba (Amikai)")
	("tr-cj-fresheye"
	 "http://mt.fresheye.com/ft_cjresult.cgi"
	 utf-8 "gen_text=%s&charset=gb2312&cjjc=cj" "Toshiba (Amikai)")
	("tr-jtw-fresheye"
	 "http://mt.fresheye.com/ft_cjresult.cgi"
	 utf-8 "gen_text=%s&charset=big5&cjjc=jc" "Toshiba (Amikai)")
	("tr-twj-fresheye"
	 "http://mt.fresheye.com/ft_cjresult.cgi"
	 utf-8 "gen_text=%s&charset=big5&cjjc=cj" "Toshiba (Amikai)")
	))

   ;; So-net translator
   ("tr-je-sonet" "http://so-net.web.transer.com/text_trans_sn.php"
    utf-8 "eid=CR-JE&text=%s" "Amikai")
   ("tr-ej-sonet" "http://so-net.web.transer.com/text_trans_sn.php"
    utf-8 "eid=CR-EJ&text=%s" "Amikai")
   ("tr-jct-sonet" "http://so-net.web.transer.com/text_trans_sn.php"
    utf-8 "eid=CR-JCT&text=%s")
   ("tr-jc-sonet" "http://so-net.web.transer.com/text_trans_sn.php"
    utf-8 "eid=CR-JC&text=%s")
   ("tr-cj-sonet" "http://so-net.web.transer.com/text_trans_sn.php"
    utf-8 "eid=CR-CJ&text=%s")
   ("tr-jk-sonet" "http://so-net.web.transer.com/text_trans_sn.php"
    utf-8 "eid=CR-JK&text=%s")
   ("tr-kj-sonet" "http://so-net.web.transer.com/text_trans_sn.php"
    utf-8 "eid=CR-KJ&text=%s")
   ;; So-net web page translator
   ("tr-je-url-sonet"
    "http://so-net.web.transer.com/url_trans_sn.php?url=%s&eid=CR-JE"
    utf-8 nil "Amikai")
   ("tr-ej-url-sonet"
    "http://so-net.web.transer.com/url_trans_sn.php?url=%s&eid=CR-EJ"
    utf-8 nil "Amikai")
   ("tr-jct-url-sonet"
    "http://so-net.web.transer.com/url_trans_sn.php?url=%s&eid=CR-JCT" utf-8)
   ("tr-jc-url-sonet"
    "http://so-net.web.transer.com/url_trans_sn.php?url=%s&eid=CR-JC" utf-8)
   ("tr-cj-url-sonet"
    "http://so-net.web.transer.com/url_trans_sn.php?url=%s&eid=CR-CJ" utf-8)
   ("tr-jk-url-sonet"
    "http://so-net.web.transer.com/url_trans_sn.php?url=%s&eid=CR-JK" utf-8)
   ("tr-kj-url-sonet"
    "http://so-net.web.transer.com/url_trans_sn.php?url=%s&eid=CR-KE" utf-8)

   ;; nifty translator
   ,@(dic-lookup-w3m-search-engine-postget
      '(("tr-ej-nifty"
	 "http://honyaku-result.nifty.com/LUCNIFTY/text/text.php"
	 utf-8 "SLANG=en&TLANG=ja&XMODE=0&SSRC=%s&txtDirection=enja"
	 "Amikai")
	("tr-je-nifty"
	 "http://honyaku-result.nifty.com/LUCNIFTY/text/text.php"
	 utf-8 "SLANG=ja&TLANG=en&XMODE=0&SSRC=%s&txtDirection=jaen"
	 "Amikai")
	("tr-cj-nifty"
	 "http://honyaku-result.nifty.com/LUCNIFTY/text/text.php"
	 utf-8 "SLANG=zh&TLANG=ja&XMODE=0&SSRC=%s&txtDirection=zhja"
	 "Amikai")
	("tr-jc-nifty"
	 "http://honyaku-result.nifty.com/LUCNIFTY/text/text.php"
	 utf-8 "SLANG=ja&TLANG=zh&XMODE=0&SSRC=%s&txtDirection=jazh"
	 "Amikai")
	("tr-twja-nifty"
	 "http://honyaku-result.nifty.com/LUCNIFTY/text/text.php"
	 utf-8 "SLANG=tw&TLANG=ja&XMODE=0&SSRC=%s&txtDirection=twja"
	 "Amikai")
	("tr-jatw-nifty"
	 "http://honyaku-result.nifty.com/LUCNIFTY/text/text.php"
	 utf-8 "SLANG=ja&TLANG=tw&XMODE=0&SSRC=%s&txtDirection=jatw"
	 "Amikai")
	("tr-kj-nifty"
	 "http://honyaku-result.nifty.com/LUCNIFTY/text/text.php"
	 utf-8 "SLANG=ko&TLANG=ja&XMODE=0&SSRC=%s&txtDirection=koja"
	 "Amikai")
	("tr-jk-nifty"
	 "http://honyaku-result.nifty.com/LUCNIFTY/text/text.php"
	 utf-8 "SLANG=ja&TLANG=ko&XMODE=0&SSRC=%s&txtDirection=jako"
	 "Amikai")
	))
   ;; nifty web page translator
   ("tr-ej-url-nifty"
    "http://honyaku-result.nifty.com/LUCNIFTY/ns/wt_ex.cgi"
    utf-8 "SLANG=en&TLANG=ja&XMODE=1&SURL=%s&webDirection=enja"
    "Amikai"
    )
   ("tr-je-url-nifty"
    "http://honyaku-result.nifty.com/LUCNIFTY/ns/wt_ex.cgi"
    utf-8 "SLANG=ja&TLANG=en&XMODE=1&SURL=%s&webDirection=jaen"
    "Amikai")
   ("tr-cj-url-nifty"
    "http://honyaku-result.nifty.com/LUCNIFTY/ns/wt_ex.cgi"
    utf-8 "SLANG=zh&TLANG=ja&XMODE=1&SURL=%s&webDirection=zhja"
    "Amikai")
   ("tr-jc-url-nifty"
    "http://honyaku-result.nifty.com/LUCNIFTY/ns/wt_ex.cgi"
    utf-8 "SLANG=ja&TLANG=zh&XMODE=1&SURL=%s&webDirection=jazh"
    "Amikai")
   ("tr-twja-url-nifty"
    "http://honyaku-result.nifty.com/LUCNIFTY/ns/wt_ex.cgi"
    utf-8 "SLANG=tw&TLANG=ja&XMODE=1&SURL=%s&webDirection=twja"
    "Amikai")
   ("tr-jatw-url-nifty"
    "http://honyaku-result.nifty.com/LUCNIFTY/ns/wt_ex.cgi"
    utf-8 "SLANG=ja&TLANG=tw&XMODE=1&SURL=%s&webDirection=jatw"
    "Amikai")
   ("tr-kj-url-nifty"
    "http://honyaku-result.nifty.com/LUCNIFTY/ns/wt_ex.cgi"
    utf-8 "SLANG=ko&TLANG=ja&XMODE=1&SURL=%s&webDirection=koja"
    "Amikai")
   ("tr-jk-url-nifty"
    "http://honyaku-result.nifty.com/LUCNIFTY/ns/wt_ex.cgi"
    utf-8 "SLANG=ja&TLANG=ko&XMODE=1&SURL=%s&webDirection=jako"
    "Amikai")

   ;; magicalgate translator
   ("tr-je-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=1&srcLang=jp&dstLang=en")
   ("tr-jk-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=1&srcLang=jp&dstLang=kr")
   ("tr-jc-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=1&srcLang=jp&dstLang=cn")
   ("tr-jptw-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=1&srcLang=jp&dstLang=tw")
   ("tr-jpbp-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=1&srcLang=jp&dstLang=bp")
   ("tr-jpde-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=1&srcLang=jp&dstLang=de")
   ("tr-jpfr-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=1&srcLang=jp&dstLang=fr")
   ("tr-jpit-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=1&srcLang=jp&dstLang=it")
   ("tr-jpes-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=1&srcLang=jp&dstLang=es")
   ("tr-ej-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=2&srcLang=jp&dstLang=en")
   ("tr-kj-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=2&srcLang=jp&dstLang=kr")
   ("tr-cj-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=2&srcLang=jp&dstLang=cn")
   ("tr-twjp-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=2&srcLang=jp&dstLang=tw")
   ("tr-bpjp-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=2&srcLang=jp&dstLang=bp")
   ("tr-dejp-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=2&srcLang=jp&dstLang=de")
   ("tr-frjp-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=2&srcLang=jp&dstLang=fr")
   ("tr-itjp-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=2&srcLang=jp&dstLang=it")
   ("tr-esjp-magicalgate" "http://221.243.5.2/impulse/TextTranslator" utf-8
    "init=init&srcText=%s&direction=2&srcLang=jp&dstLang=es")

   ;; infoseek translator
   ,@(dic-lookup-w3m-search-engine-postget
      '(("tr-ej-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=en&original=%s&selector=0&submit=　翻訳　"
	 "Cross Language")
	("tr-je-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=en&original=%s&selector=1&submit=　翻訳　"
	 "Cross Language")
	("tr-kj-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=ko&original=%s&selector=0&submit=　翻訳　"
	 "Changshin Soft (CSLI)")
	("tr-jk-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=ko&original=%s&selector=1&submit=　翻訳　"
	 "Changshin Soft (CSLI)")
	("tr-cj-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=zh&original=%s&selector=0&submit=　翻訳　"
	 "Cross Language")
	("tr-jc-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=zh&original=%s&selector=1&submit=　翻訳　"
	 "Cross Language")
	("tr-frjp-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=fr&original=%s&selector=0&submit=　翻訳　")
	("tr-jpfr-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=fr&original=%s&selector=1&submit=　翻訳　")
	("tr-dejp-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=de&original=%s&selector=0&submit=　翻訳　")
	("tr-jpde-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=de&original=%s&selector=1&submit=　翻訳　")
	("tr-itjp-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=it&original=%s&selector=0&submit=　翻訳　")
	("tr-jpit-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=it&original=%s&selector=1&submit=　翻訳　")
	("tr-esjp-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=es&original=%s&selector=0&submit=　翻訳　")
	("tr-jpes-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=es&original=%s&selector=1&submit=　翻訳　")
	("tr-ptjp-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=pt&original=%s&selector=0&submit=　翻訳　")
	("tr-jppt-infoseek" "http://translation.infoseek.co.jp/" utf-8
	 "ac=Text&lng=pt&original=%s&selector=1&submit=　翻訳　")
	))

   ;; 訳してねっと web page translator
   ("tr-ej-url-yakushite.net"
    "http://www.yakushite.net/cgi-bin/WebObjects/YakushiteNet.woa/wa/TranslateDirectAction/defaultTrans?direction=LR&_COMMUNITY_ID=900002&textArea=%s&isRecommend=5.0.3.3.1.0.5.0.0.1.15&5.0.3.3.1.0.5.0.0.1.19.1=翻訳" utf-8)
   ("tr-je-url-yakushite.net"
    "http://www.yakushite.net/cgi-bin/WebObjects/YakushiteNet.woa/wa/TranslateDirectAction/defaultTrans?direction=RL&_COMMUNITY_ID=900002&textArea=%s&isRecommend=5.0.3.3.1.0.5.0.0.1.15&5.0.3.3.1.0.5.0.0.1.19.1=翻訳" utf-8)
   ("tr-cj-url-yakushite.net"
    "http://www.yakushite.net/cgi-bin/WebObjects/ChinaYakushiteNet.woa/wa/TranslateDirectAction/defaultTrans?direction=LR&_COMMUNITY_ID=900002&textArea=%s&isRecommend=5.0.3.3.1.0.5.0.0.1.15&5.0.3.3.1.0.5.0.0.1.19.1=翻訳" utf-8)
   ("tr-jc-url-yakushite.net"
    "http://www.yakushite.net/cgi-bin/WebObjects/ChinaYakushiteNet.woa/wa/TranslateDirectAction/defaultTrans?direction=RL&_COMMUNITY_ID=900002&textArea=%s&isRecommend=5.0.3.3.1.0.5.0.0.1.15&5.0.3.3.1.0.5.0.0.1.19.1=翻訳" utf-8)
   ("tr-ej-url-yakushite.net-put"
    "http://www.yakushite.net/cgi-bin/WebObjects/YakushiteNet.woa/wa/TranslateDirectAction/defaultTrans"
    utf-8 "direction=LR&_COMMUNITY_ID=900002&textArea=%s&isRecommend=5.0.3.3.1.0.5.0.0.1.15&5.0.3.3.1.0.5.0.0.1.19.1=翻訳")
   ("tr-je-url-yakushite.net-put"
    "http://www.yakushite.net/cgi-bin/WebObjects/YakushiteNet.woa/wa/TranslateDirectAction/defaultTrans"
    utf-8 "direction=RL&_COMMUNITY_ID=900002&textArea=%s&isRecommend=5.0.3.3.1.0.5.0.0.1.15&5.0.3.3.1.0.5.0.0.1.19.1=翻訳")
   ("tr-cj-url-yakushite.net-put"
    "http://www.yakushite.net/cgi-bin/WebObjects/ChinaYakushiteNet.woa/wa/TranslateDirectAction/defaultTrans"
    utf-8 "direction=LR&_COMMUNITY_ID=900002&textArea=%s&isRecommend=5.0.3.3.1.0.5.0.0.1.15&5.0.3.3.1.0.5.0.0.1.19.1=翻訳")
   ("tr-jc-url-yakushite.net-put"
    "http://www.yakushite.net/cgi-bin/WebObjects/ChinaYakushiteNet.woa/wa/TranslateDirectAction/defaultTrans"
    utf-8 "direction=RL&_COMMUNITY_ID=900002&textArea=%s&isRecommend=5.0.3.3.1.0.5.0.0.1.15&5.0.3.3.1.0.5.0.0.1.19.1=翻訳")
   ))

;; Glosbe 多言語オンライン辞書、翻訳メモリ
(defvar dic-lookup-w3m-glosbe-langs
  '(("ja" "en") nil)
  ;;'(("ja" "en") ("en" "ko" "zh" "es"))
  "*glosbeで有効にする言語のリスト。
\((LANGS1) (LANGS2))
LANGS1、LANGS2は言語名のリスト。LANGS1 X LANGS2の組み合わせで翻訳
ができるようにする。
nilの場合はすべての言語を対象にする。nil X nilは起動が遅くなる。")

(let* ((langs
	'(("xno" . "anglo-norman")
	  ("bar" . "bavarian")
	  ("swh" . "coastal swahili")
	  ("sh" . "hbs")
	  ("pes" . "iranian persian")
	  ("ses" . "koyraboro senni songhai")
	  ("cmn" . "mandarin")
	  ("nan" . "min nan chinese")
	  ("nov" . "novial")
	  ("pms" . "piemontese")
	  ("azb" . "south azerbaijani")
	  ("arb" . "standard arabic")
	  ("vec" . "venetian")
	  ("wym" . "wymysorys")
	  ("yua" . "yucateco")
	  ("is" . "アイスランド語")
	  ("ga" . "アイルランド語")
	  ("ast" . "アストゥリアス語")
	  ("az" . "アゼルバイジャン語")
	  ("af" . "アフリカーンス語")
	  ("an" . "アラゴン語")
	  ("ar" . "アラビア語")
	  ("sq" . "アルバニア語")
	  ("hy" . "アルメニア語")
	  ("it" . "イタリア語")
	  ("yi" . "イディッシュ語")
	  ("io" . "イド語")
	  ("ia" . "インターリングア語")
	  ("id" . "インドネシア語")
	  ("cy" . "ウェールズ語")
	  ("uk" . "ウクライナ語")
	  ("uz" . "ウズベク語")
	  ("ur" . "ウルドゥー語")
	  ("et" . "エストニア語")
	  ("eo" . "エスペラント語")
	  ("oc" . "オック語")
	  ("nl" . "オランダ語")
	  ("kk" . "カザフ語")
	  ("kha" . "カシ語")
	  ("ca" . "カタロニア語")
	  ("krc" . "カラチャイ語")
	  ("gl" . "ガリシア語")
	  ("el" . "ギリシャ語")
	  ("gu" . "グジャラート語")
	  ("km" . "クメール語")
	  ("crh" . "クリミア・タタール語")
	  ("ka" . "グルジア語")
	  ("ku" . "クルド語")
	  ("hr" . "クロアチア語")
	  ("sa" . "サンスクリット語")
	  ("scn" . "シチリア語")
	  ("sv" . "スウェーデン語")
	  ("zu" . "ズールー語")
	  ("gd" . "スコットランド・ゲール語")
	  ("sco" . "スコットランド語")
	  ("es" . "スペイン語")
	  ("sk" . "スロバキア語")
	  ("sl" . "スロベニア語")
	  ("sw" . "スワヒリ語")
	  ("sr" . "セルビア語")
	  ("th" . "タイ語")
	  ("tl" . "タガログ語")
	  ("tg" . "タジク語")
	  ("tt" . "タタール語")
	  ("ta" . "タミール語")
	  ("cs" . "チェコ語")
	  ("te" . "テルグ語")
	  ("da" . "デンマーク語")
	  ("de" . "ドイツ語")
	  ("tk" . "トルクメン語")
	  ("tr" . "トルコ語")
	  ("nv" . "ナバホ語")
	  ("nap" . "ナポリ語")
	  ("nn" . "ノルウェー語[ニーノシュク]")
	  ("nb" . "ノルウェー語[ブークモール]")
	  ("ht" . "ハイチ語")
	  ("eu" . "バスク語")
	  ("pap" . "パピアメント語")
	  ("haw" . "ハワイ語")
	  ("hu" . "ハンガリー語")
	  ("hil" . "ヒリガイノン語")
	  ("hi" . "ヒンディー語")
	  ("fil" . "フィリピノ語")
	  ("fi" . "フィンランド語")
	  ("fo" . "フェロー語")
	  ("fr" . "フランス語")
	  ("fy" . "フリジア語")
	  ("bg" . "ブルガリア語")
	  ("br" . "ブルトン語")
	  ("vi" . "ベトナム語")
	  ("he" . "ヘブライ語")
	  ("be" . "ベラルーシ語")
	  ("fa" . "ペルシア語")
	  ("bn" . "ベンガル語")
	  ("pl" . "ポーランド語")
	  ("bs" . "ボスニア語")
	  ("vo" . "ボラピュク語")
	  ("pt" . "ポルトガル語")
	  ("mk" . "マケドニア語")
	  ("mr" . "マラーティー語")
	  ("ml" . "マラヤーラム語")
	  ("mt" . "マルタ語")
	  ("ms" . "マレー語")
	  ("gv" . "マン島語")
	  ("mn" . "モンゴル語")
	  ("yo" . "ヨルバ語")
	  ("lo" . "ラオ語")
	  ("la" . "ラテン語")
	  ("lv" . "ラトビア語")
	  ("lt" . "リトアニア語")
	  ("li" . "リンブルフ語")
	  ("ro" . "ルーマニア語")
	  ("lb" . "ルクセンブルク語")
	  ("rm" . "レト・ロマン語")
	  ("ru" . "ロシア語")
	  ("rom" . "ロマーニー語")
	  ("en" . "英語")
	  ("ko" . "韓国語")
	  ("non" . "古ノルド語")
	  ("fro" . "古フランス語")
	  ("ang" . "古代英語")
	  ("frm" . "中期フランス語")
	  ("zh" . "中国語")
	  ("nds" . "低地ドイツ語、低地サクソン語")
	  ("ja" . "日本語")
	  ("se" . "北サーミ語")
	  ))
       (langs1 (or (nth 0 dic-lookup-w3m-glosbe-langs)
		   (mapcar 'car langs)))
       (langs2 (or (nth 1 dic-lookup-w3m-glosbe-langs)
		   (mapcar 'car langs))))
  (dolist (l1 langs1)
    (dolist (l2 langs2)
      (dolist (arg (list (list l1 l2) (list l2 l1)))
	(apply
	 #'(lambda (l1 l2)
	    (add-to-list
	     'dic-lookup-w3m-search-engine-alist
	     (list
	      (format "%s%s-glosbe" l1 l2)
	      (format
	       "http://ja.glosbe.com/%s/%s/%%s"
	       l1 l2)
	      'utf-8 nil
	      (concat (assoc-default l1 langs)
		      "-" (assoc-default l2 langs)))))
	 arg)))))

;; google translator
(defvar dic-lookup-w3m-google-translator-langs
  '(("ja" "en") nil)
  ;;'(("ja" "en") ("en" "ko" "zh-CN" "es"))
  "*google翻訳で有効にする言語のリスト。
\((LANGS1) (LANGS2))
LANGS1、LANGS2は言語名のリスト。LANGS1 X LANGS2の組み合わせで翻訳
ができるようにする。
nilの場合はすべての言語を対象にする。nil X nilは起動が遅くなる。")

(let* ((langs
	'(("ar" . "アラビア語")
	  ("sq" . "アルバニア語")
	  ("it" . "イタリア語")
	  ("id" . "インドネシア語")
	  ("uk" . "ウクライナ語")
	  ("et" . "エストニア語")
	  ("nl" . "オランダ語")
	  ("ca" . "カタロニア語")
	  ("gl" . "ガリシア語")
	  ("el" . "ギリシャ語")
	  ("hr" . "クロアチア語")
	  ("sv" . "スウェーデン語")
	  ("es" . "スペイン語")
	  ("sk" . "スロバキア語")
	  ("sl" . "スロベニア語")
	  ("sr" . "セルビア語")
	  ("th" . "タイ語")
	  ("tl" . "タガログ語")
	  ("cs" . "チェコ語")
	  ("da" . "デンマーク語")
	  ("de" . "ドイツ語")
	  ("tr" . "トルコ語")
	  ("no" . "ノルウェー語")
	  ("hu" . "ハンガリー語")
	  ("hi" . "ヒンディー語")
	  ("fi" . "フィンランド語")
	  ("fr" . "フランス語")
	  ("bg" . "ブルガリア語")
	  ("vi" . "ベトナム語")
	  ("iw" . "ヘブライ語")
	  ("pl" . "ポーランド語")
	  ("pt" . "ポルトガル語")
	  ("mt" . "マルタ語")
	  ("lv" . "ラトビア語")
	  ("lt" . "リトアニア語")
	  ("ro" . "ルーマニア語")
	  ("ru" . "ロシア語")
	  ("en" . "英語")
	  ("ko" . "韓国語")
	  ("zh-CN" . "中国語(簡体)")
	  ("zh-TW" . "中国語(繁体)")
	  ("ja" . "日本語")
	  ))
       (langs1 (or (nth 0 dic-lookup-w3m-google-translator-langs)
		   (mapcar 'car langs)))
       (langs2 (or (nth 1 dic-lookup-w3m-google-translator-langs)
		   (mapcar 'car langs))))
  (dolist (l1 langs1)
    (unless (equal l1 "zh-TW")
      (dolist (l2 langs2)
	(unless (equal l2 l1)
	  (dolist (arg (list (list l1 l2) (list l2 l1)))
	    (apply
	     #'(lambda (l1 l2)
		 (add-to-list
		  'dic-lookup-w3m-search-engine-alist
		  (list
		   (format "tr-%s%s-google" l1 l2)
		   "https://translate.google.com/" 'utf-8
		   (format "sl=%s&tl=%s&js=n&prev=_t&hl=ja&ie=utf-8&text=%%s"
			   l1 l2)
		   (concat (assoc-default l1 langs)
			   "-" (assoc-default l2 langs))))
		 (add-to-list		;url翻訳は動かない
		  'dic-lookup-w3m-search-engine-alist
		  (list
		   (format "tr-%s%s-url-google" l1 l2)
		   (format
		    "https://translate.google.com/translate?sl=%s&tl=%s&js=n&prev=_t&ie=UTF-8&u=%%s"
		    l1 l2)
		   'utf-8 nil
		   (concat (assoc-default l1 langs)
			   "-" (assoc-default l2 langs)))))
	     arg)))))))

;; google translator (aliases)
(defvar dic-lookup-w3m-search-engine-aliases '())
(mapc
 #'(lambda (e) (add-to-list 'dic-lookup-w3m-search-engine-aliases e))
 '(("tr-ej-google" "tr-enja-google")
   ("tr-je-google" "tr-jaen-google")
   ("tr-cj-google" "tr-zh-CNja-google")
   ("tr-jc-google" "tr-jazh-CN-google")
   ("tr-kj-google" "tr-koja-google")
   ("tr-jk-google" "tr-jako-google")))

(defvar dic-lookup-w3m-filter-do-show-candidates-heading " &nbsp;候補: "
  "*単語の候補リストの前に表示する見出し。")

(defvar dic-lookup-w3m-filter-related-links-heading " 関連: "
  "*関連サイトのリストの前に表示する見出し。")

(defvar dic-lookup-w3m-favorite-ej-engine "ej-excite")

(defvar dic-lookup-w3m-filter-disable-translation-anchor nil
  "*webページに翻訳ボタンをつけるかどうかのフラグ。
non-nilならページ翻訳ボタンを付けない。
nilなら`dic-lookup-w3m-filter-translation-anchor'を呼び出してwebページ
に翻訳ボタンをつける。")

(eval-after-load "w3m-filter"
  '(mapc
    #'(lambda (elem)
       (add-to-list 'w3m-filter-rules elem))
    (reverse
     `(
       ,(unless dic-lookup-w3m-filter-disable-translation-anchor
	  '("" dic-lookup-w3m-filter-translation-anchor)) ; ページ翻訳ボタン

       ;; yahoo dic
       ("\\`http://dic\\.\\(search\\.\\)?yahoo\\.co\\.jp//?d?search"
	(w3m-filter-delete-regions "<body[^>]*>" "<!-- /navi -->" t nil t)
	(w3m-filter-delete-regions "<body[^>]*>" "<div id=\"mIn\">" t t t)
	(w3m-filter-delete-regions "<!-- QR -->" "</body>" nil t)
	(w3m-filter-replace-regexp
	 "<img src=\"http://img.yahoo.co.jp/images/clear.gif\"[^>]*>" "")
	(w3m-filter-replace-regexp
	 "<img src=\"http://i.yimg.jp/images/clear.gif\"[^>]*>" "")
	(dic-lookup-w3m-filter-eword-anchor "ej-yahoo")
	)
       ("\\`http://dic\\.search\\.yahoo\\.co\\.jp//?dsearch.*dic_id=jj"
	dic-lookup-w3m-filter-related-links "jj-yahoo" jj)
       ("\\`http://dic\\.search\\.yahoo\\.co\\.jp//?dsearch.*dic_id=ejje"
	dic-lookup-w3m-filter-related-links "ej-yahoo" ej)
       ;; ("\\`http://dic\\.search\\.yahoo\\.co\\.jp/dsearch.*dic_id=ejje"
       ;; 	dic-lookup-w3m-filter-convert-phonetic-symbol
       ;; 	dic-lookup-w3m-filter-yahoo-ej2-symbol-alist
       ;; 	"<img src=\"[^\"]+/\\([a-z0-9]+\\)\\.gif\"[^>]*>")
       ;; ("\\`http://dic\\.search\\.yahoo\\.co\\.jp/dsearch.*dtype=\\(jj\\|ejje\\)"
       ;; 	dic-lookup-w3m-filter-convert-phonetic-symbol
       ;; 	dic-lookup-w3m-filter-yahoo-ej1-symbol-alist
       ;; 	"<img src=\"[^\"]+/\\([A-Z0-9_]+\\)\\.gif\"[^>]*>")
       ("\\`http://dic\\.search\\.yahoo\\.co\\.jp//?dsearch"
	dic-lookup-w3m-filter-show-candidates "ej-yahoo")

       ;; excite dic
       ("\\`http://www\\.excite\\.co\\.jp/dictionary/japanese/\\?search="
	(w3m-filter-replace-regexp "<p>le=\"\">" "<p>")
	(w3m-filter-replace-regexp "<br [^>]*</p>" "<br /></p>")
	)
       ("\\`http://www\\.excite\\.co\\.jp/dictionary/.*search="
	(dic-lookup-w3m-filter-excite-jump-to-1stcontent
	 "http://www.excite.co.jp%s"
	 "<a href=\"\\(/dictionary/.*/\\?search=[^>]*\\(block\\|itemid\\|;id\\)=[^>]*\\)\">" 1)
	(w3m-filter-delete-regions
	 "<body>" "<div class=\"dictionary_history\">" t t)
	(w3m-filter-delete-regions
	 "<div class=\"dictionary_history\">" "<div class=\"content\">" t t) ;最近検索した語句
	(w3m-filter-delete-regions
	 "<div class=\"content\">" "<div class=\"wordDetails\">" nil t)
	(w3m-filter-delete-regions
	 "<div class=\"content cnja\">" "<div class=\"wordDetails\">" nil t)
	(w3m-filter-replace-regexp
	 "<div class=\"wordDetails\">" "<br><div class=\"wordDetails\">")
	(w3m-filter-replace-regexp
	 "\\(<div class=\"dictionary_history\">\\)" "\\1<br>")
	(w3m-filter-delete-regions "<body>" "<div class=\"section\">" t t)
	(w3m-filter-delete-regions
	 "<div class=\"sectionAside\">" "</body>" nil t t)
	(w3m-filter-replace-regexp
	 "<img src=\"?http://image\.excite\.co\.jp/jp/1pt\.gif\"?[^>]*>" "")
	(dic-lookup-w3m-filter-eword-anchor "ej-excite")
	)
       ("\\`http://www\\.excite\\.co\\.jp/dictionary/.*english.*/.*search="
	(dic-lookup-w3m-filter-related-links "ej-excite" ej)
	(dic-lookup-w3m-filter-convert-phonetic-symbol
	 dic-lookup-w3m-filter-excite-ej-symbol-alist
	 "<img src=\"http://dictionary\\.eiwa\\.excite\\.co\\.jp/images/\\(NEW_EJJE\\|COMP_EJ\\)/gaiji/\\([a-z0-9]+\\)\\.gif\"[^>]*>"
	 2)
	)
       ("\\`http://www\\.excite\\.co\\.jp/dictionary/japanese/\\?search="
	(w3m-filter-replace-regexp
	 "<span class=\"NetDicItemLink\" ItemID=\"\\([^\"]+\\)\">\\(\\([^<]+\\).*\</span>\\)"
	 "<a href=\"./?search=\\3&itemid=\\1\">\\2</a>")
	(dic-lookup-w3m-filter-convert-phonetic-symbol
	 dic-lookup-w3m-filter-excite-jj-symbol-alist
	 "<img src=\"http://b2b\\.dejizo\\.jp/Resource\\.aspx\\?set=.*&amp;name=\\([A-Za-z_0-9]+\\)[^>]*>")
	(w3m-filter-replace-regexp
	 "<img src=\"http://b2b\\.dejizo\\.jp/Resource\\.aspx\\?set=unicode&amp;name=\\([^&\"]+\\)[^>]*>" "&#x\\1\;")
	(w3m-filter-replace-regexp "</div><div style=\"margin-left:1.2em;\">" "")
	(dic-lookup-w3m-filter-related-links "jj-excite" jj)
	)
       ("\\`http://www\\.excite\\.co\\.jp/dictionary/chinese_japanese/\\?search="
	(dic-lookup-w3m-filter-related-links "cj-excite" cj)
	(dic-lookup-w3m-filter-convert-phonetic-symbol
	 dic-lookup-w3m-filter-excite-cj-symbol-alist
	 "<img src=\"?http://image\\.excite\\.co\\.jp/jp/dictionary/\\(pinyin\\|chinese_japanese\\)/\\([a-z_0-9]+\\)\\.gif\"?[^>]*>"
	 2)
	)
       ("\\`http://www\\.excite\\.co\\.jp/dictionary/japanese_chinese/\\?search="
	(dic-lookup-w3m-filter-related-links "jc-excite" cj)
	(w3m-filter-replace-regexp
	 "\\(<img src=\"http://image\\.excite\\.co\\.jp/jp/dictionary/japanese_chinese/\\(yakugo\\|youyaku\\)\.gif\"[^>]*/>\\)\\([^<]+\\)\\(&nbsp;\\)"
	 "\\1<a href=\"/dictionary/chinese_japanese/?search=\\3\">\\3</a>\\4")
	(dic-lookup-w3m-filter-convert-phonetic-symbol
	 dic-lookup-w3m-filter-excite-cj-symbol-alist
	 "<img src=\"?http://image\\.excite\\.co\\.jp/jp/dictionary/\\(pinyin\\|japanese_chinese\\)/\\([a-z_0-9]+\\)\\.gif\"?[^>]*>"
	 2)
	)
       ("\\`http://www\\.excite\\.co\\.jp/dictionary/.*search="
	dic-lookup-w3m-filter-show-candidates "ej-excite")

       ;; alc
       ("\\`http://eow\\.alc\\.co\\.jp/[^/]+/UTF-8"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div id=\"resultsArea\">" t nil t t)
	(w3m-filter-delete-regions "<span class=\"kana\">" "</span>")
	(dic-lookup-w3m-filter-related-links "ej-alc" ej)
	(dic-lookup-w3m-filter-eword-anchor "ej-alc")
	)

       ;; alc business term dic
       ("\\`http://home\\.alc\\.co\\.jp/db/owa/bdicn_sch"
	w3m-filter-delete-regions
	"<body bgcolor=\"#FFFFFF\">" "<!--▲input_form-->" t)

       ;; webster
       ("\\`http://www\\.merriam-webster\\.com/\\(dictionary\\|thesaurus\\)/.+"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div class=\"definition\">" t t t)
	(w3m-filter-delete-regions
	 "<div class=\"browse learn_more\">" "</body>")
	(w3m-filter-delete-regions
	 "<div id=\"page_wrapper\">" "<div class=\"page_content\">")
	(dic-lookup-w3m-filter-related-links "ee-webster" ee)
	(dic-lookup-w3m-filter-eword-anchor "ee-webster")
	(dic-lookup-w3m-filter-show-candidates "ee-webster")
	)

       ;; cambridge
       ("\\`http://dictionary\\.cambridge\\.org/dictionary/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div class=\"cdo-section\">" t t t)
	(dic-lookup-w3m-filter-related-links "ee-cambridge" ee)
	(dic-lookup-w3m-filter-eword-anchor "ee-cambridge")
	(dic-lookup-w3m-filter-show-candidates "ee-cambridge")
	)

       ;; macmillan
       ("\\`http://www\\.macmillandictionary\\.com/dictionary/british/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div id=\"headwordleft\">" t t t)
	(w3m-filter-delete-regions
	 "<span class=\"headword-definition\">"
	 "<!-- End of DIV thesaurus-layer-->" t t t)
	(w3m-filter-replace-regexp
	 "<img [^>]*src=[^>]*data-src-mp3=\\(\"[^\"]+\"\\)[^>]*>"
	 "<a href=\\1>♪</a>")
	(dic-lookup-w3m-filter-related-links "ee-macmillan" ee)
	(dic-lookup-w3m-filter-eword-anchor "ee-macmillan")
	(dic-lookup-w3m-filter-show-candidates "ee-macmillan")
       	)
       ("\\`http://www.macmillandictionary.com/spellcheck/british/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div id=\"search-results\">" t t t)
	)

       ;; yahoo.com
       ("\\`http://education\\.yahoo\\.com/reference/[^/]+/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<p class='bodytext'>" t t t)
	(w3m-filter-delete-regions
	 "<font face=\"arial\" size=\"-2\">Visit our partner's site</font>"
	 "\\'" nil nil nil t)
	(w3m-filter-delete-regions
	 "<body[^>]*>"
	 "<div id=\"yedusearchresultspaginationtop\"[^>]*>" t t t t)
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div id=\"yeduarticlenavigationtop\"[^>]*>" t t t t)
	(w3m-filter-replace-regexp
	 "<img [^>]*src=\"http://l.yimg.com/a/i/edu/ref/ahd/t/pron.jpg\"[^<]*>"
	 "♪")
	(dic-lookup-w3m-filter-related-links "ee-yahoo.com" ee)
	(dic-lookup-w3m-filter-eword-anchor "ee-yahoo.com")
	(dic-lookup-w3m-filter-show-candidates "ee-yahoo.com")
	)
       ("\\`http://education\\.yahoo\\.com/reference/dict_en_es/"
	(w3m-filter-delete-regions "<body[^>]*>" "Your search: " t t t)
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div id=\"yeduarticlenavigationtop\"[^>]*>" t t t t)
	)

       ;; kotonoha
       ("\\`http://www\\.kotonoha\\.gr\\.jp/shonagon"
	(w3m-filter-delete-regions
	 "<div id=\"wrapper\">" "<!-- END of header -->")
	(w3m-filter-delete-regions
	 "<div id=\"headerB\">" "<p>検索文字列：" t t)
	(w3m-filter-delete-regions
	 "<div id=\"headerB\">" "<h2>検索結果</h2>" t t)
	)
       ("\\`http://www\\.kotonoha\\.gr\\.jp/shonagon"
	(w3m-filter-replace-regexp "class=\"cell01\"" "align=\"right\"")
	(w3m-filter-replace-regexp
	 "<td class=\"cell02\">\\([^<]*\\)</td>"
	 "<td class=\"cell02\"><strong>\\1</strong></td>")
	(w3m-filter-replace-regexp "<td\\([ >]\\)" "<td nowrap\\1")
	)
       ("\\`http://www\\.kotonoha\\.gr\\.jp/shonagon/?$"
	dic-lookup-w3m-filter-refresh-url
	"http://www.kotonoha.gr.jp/shonagon/search_form")

       ;; 青空文庫 日本語用例検索
       ("\\`http://www.tokuteicorpus.jp/team/jpling/kwic/search.cgi"
	(w3m-filter-replace-regexp "<font color=\"crimson\">" "<strong>")
	(w3m-filter-replace-regexp "</font>" "</strong>")
	)

       ;; erek corpus
       ("\\`http://erek\\.ta2o\\.net/"
	(w3m-filter-replace-regexp
	 "<div class=\"kwicright\">\\([^<]*\\)</div>" "<span>\\1</span>")
	(w3m-filter-replace-regexp
	 "<div class=\"kwiccenter\"\\(.*\n.*\\)</div>" "<span\\1</span>")
	(w3m-filter-replace-regexp
	 "<div class=\"kwicleft\">\\([^<]*\\)</div>" "<span>\\1</span>")
	)
       ;; jrek corpus
       ("\\`http://jrek\\.ta2o\\.net/"
	(w3m-filter-replace-regexp
	 "<td class=\"kwicright\"\\([^>]*\\)>" "<td align=\"left\"\\1 nowrap>")
	(w3m-filter-replace-regexp
	 "<td class=\"kwiccenter\"\\([^>]*\\)>" "<td align=\"center\"\\1 nowrap>")
	(w3m-filter-replace-regexp
	 "<td class=\"kwicleft\"\\([^>]*\\)>" "<td align=\"right\"\\1 nowrap>")
	(w3m-filter-replace-regexp
	 "<span class=\"sortid\">[0-9]*</span>" "")
	)

       ;; bnc corpus
       ("\\`http://sara\\.natcorp\\.ox\\.ac\\.uk/cgi-bin/saraWeb\\?qy=.*"
	dic-lookup-w3m-filter-eword-anchor dic-lookup-w3m-favorite-ej-engine)

       ;; Dictionary.com
       ("http://\\(thesaurus\\|dictionary\\)\\.reference\\.com/browse/"
	(w3m-filter-delete-regions
	 "<body onload=\"initpage();\">" "<div id=\"contentResults\">" t t)
	(dic-lookup-w3m-filter-related-links "thesaurus-rogets" ej)
	)

       ;; lsd
       ("\\`http://lsd\\.pharm\\.kyoto-u\\.ac\\.jp/cgi-bin/lsdproj/etoj-cgi04\\.pl"
	dic-lookup-w3m-filter-eword-anchor "ej-lsd")

       ;; RNN時事英語辞典
       ("\\`http://rnnnews\\.jp/"
	(w3m-filter-delete-regions
	 "<body>" "<div id=\"body\">" t t)
	(w3m-filter-replace-regexp
	 "<img src=\"../../img/related.gif\"[^>]*>" "関連:")
	(dic-lookup-w3m-filter-eword-anchor dic-lookup-w3m-favorite-ej-engine)
	)

       ;; bitex
       ("\\`http://bitex-cn\\.com/search_result\\.php"
	(w3m-filter-delete-regions
	 "<body>" "<div class=\"center03\">" t t)
	(dic-lookup-w3m-filter-related-links "cj-bitex" cj)
	)

       ;; 敦煌辞海
       ("\\`http://www\\.onlinedic\\.com/search\\.php"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<!-- Main -->" t t t)
	(w3m-filter-delete-regions
	 "<td width=\"250\" bgcolor=\"#E5F2FB\" valign=\"top\">"
	 "</body>" nil t)
	(dic-lookup-w3m-filter-related-links "cj-tonko-jikai" cj)
	(dic-lookup-w3m-filter-convert-phonetic-symbol
	 dic-lookup-w3m-filter-onlinedic-symbol-alist
	 "<img [^>]+images/\\([a-z0-9_]+\\)\\.gif[^>]*>")
	(w3m-filter-replace-regexp
	 "\\(<\\(table\\|td\\) [^>]*\\)\\(width=\"[0-9]+\"\\)\\([^>]*>\\)"
	 "\\1\\4")
	(w3m-filter-replace-regexp "</?font[^>]*>" "")
	(w3m-filter-replace-regexp
	 "\\(<td class=\"line1\">中国語：</td><td class=\"line2\">\\)\\([^<]+\\)</td>"
	 "\\1\\2 ⇒<a href=\"http://mandarinspot.com/annotate?text=\\2&spaces=1&phs=pinyin&show=both\">pinyin</a>")
	)

       ;; 楽訳中国語辞書
       ("\\`http://www\\.jcdic\\.com/search\\.php"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div class='result'>" t t t)
	(w3m-filter-delete-regions
	 "<!--Adsense開始-->" "<!-- Footer -->")
	(dic-lookup-w3m-filter-related-links "cj-tonko-jikai" cj)
	(dic-lookup-w3m-filter-convert-phonetic-symbol
	 dic-lookup-w3m-filter-onlinedic-symbol-alist
	 "<img [^>]+images/\\([a-z0-9_]+\\)\\.gif[^>]*>")
	)
       ("\\`http://www\\.jcdic\\.com/chinese_convert/index\\.php"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<td class=\"redword\">&nbsp;</td>" t nil t)
	(w3m-filter-delete-regions "</form>" "</body>" t t)
	(dic-lookup-w3m-filter-conv-pinyin
	 "<div class=\"convert\">\\(.*\\)</div>")
	)

       ;; MandarinSpot
       ("http://mandarinspot\\.com/annotate"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div id=\"annotated\">" t nil t)
	(w3m-filter-delete-regions
	 "<div class=\"mid\" style=\"\">" "</body" nil t)
	(dic-lookup-w3m-filter-word-anchor
	 "cj-goo" "<div class=\"zh\">\\([^<]*\\)</div>" 1)
	(w3m-filter-replace-regexp
	 "<div class=\"\\(py\\|zh\\)\">\\([^<]*\\)</div>"
	 "<span class=\"\\1\">\\2 </span>")
	)

       ;; PinYin.JP
       ("http://pinyin\\.jp/hz2py\\.cgi"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<textarea name=hz cols=50 rows=5>" t t t)
	(w3m-filter-delete-regions
	 "</table>" "</body" nil t)
	(w3m-filter-delete-regions
	 "</textarea>"
	 "変換後のピンイン</td><td><table class=t2><tr><td>" t)
	(w3m-filter-replace-regexp
	 "<textarea name=hz cols=50 rows=5>\\([^<]*\\)</textarea>" "\\1<br>")
	)
       ;; hjenglish 中日
       ("\\`http://dict\\.hjenglish\\.com/.*type=cj"
	dic-lookup-w3m-filter-related-links "cj-hjenglish" cj)
       ("\\`http://dict\\.hjenglish\\.com/.*type=jc"
	dic-lookup-w3m-filter-related-links "jc-hjenglish" cj)
       ("\\`http://dict\\.hjenglish\\.com/"
	dic-lookup-w3m-filter-convert-phonetic-symbol
	dic-lookup-w3m-filter-hjenglish-symbol-alist
	"<img [^>]+/images/\\([a-z0-9_]+\\)\\.gif[^>]*>")

       ;; inforseek dic
       ("\\`http://dictionary\\.infoseek\\.ne\\.jp/"
       	w3m-filter-delete-regions
       	"<body[^>]*>"
       	"\\(<ul class=\"search_list\">\\|<div class=\"word_block\">\\)"
       	t t t t)
       ("\\`http://dictionary\\.infoseek\\.ne\\.jp/"
	(dic-lookup-w3m-filter-show-candidates "ej-infoseek")
	(dic-lookup-w3m-filter-eword-anchor "ej-infoseek")
	)
       ("\\`http://dictionary\\.infoseek\\.ne\\.jp/ejword"
       	dic-lookup-w3m-filter-related-links
	"ej-infoseek" ej "http://dictionary.infoseek.ne.jp/ejword/%s")
       ("http://dictionary\\.infoseek\\.ne\\.jp/jeword"
       	dic-lookup-w3m-filter-related-links
	"je-infoseek" ej "http://dictionary.infoseek.ne.jp/jeword/%s")
       ("\\`http://dictionary\\.infoseek\\.ne\\.jp/word"
       	dic-lookup-w3m-filter-related-links
	"jj-infoseek" jj "http://dictionary.infoseek.ne.jp/word/%s")
       ("\\`http://dictionary\\.infoseek\\.ne\\.jp/"
	(dic-lookup-w3m-filter-convert-phonetic-symbol
	 dic-lookup-w3m-filter-yahoo-ej1-symbol-alist
	 "<img name=\"[^\"]+\" src=\"/lang/g/pej4/\\([A-Z0-9_]+\\).png\"/>")
       	)

       ;; kotobank
       ("\\`http://kotobank\\.jp/"
       	(w3m-filter-delete-regions
	 "<body[^>]*>"
	 "\\(<div class=\"full\">\\|<ol id=\"wordAgree\">\\|<div class=\"word_dic\">\\)" t t t t)
	(w3m-filter-replace-regexp "<img [^>]*src=\"/i/word.png\"[^>]*>" "")
	(w3m-filter-delete-regions
	 "\\(<div id=\"banner_app\">\\|<h2 id=\"word_connect\">\\|<p id=\"word_bottom\">\\)"
	 "</body>" nil t t nil)
	)
       ("\\`http://kotobank\\.jp/ejword/"
	(dic-lookup-w3m-filter-convert-phonetic-symbol
	 dic-lookup-w3m-filter-yahoo-ej1-symbol-alist
	 "<img name=\"[^\"]+\" src=\"/lang/g/pej4/\\([A-Z0-9_]+\\).png\"/>")
	(dic-lookup-w3m-filter-eword-anchor "ej-kotobank")
	(dic-lookup-w3m-filter-related-links
	 "ej-kotobank" ej "http://kotobank.jp/ejword/%s")
	(dic-lookup-w3m-filter-show-candidates "ej-kotobank")
       	)
       ("\\`http://kotobank\\.jp/jeword/"
	(dic-lookup-w3m-filter-eword-anchor "ej-kotobank")
	(dic-lookup-w3m-filter-related-links
	 "ej-kotobank" ej "http://kotobank.jp/jeword/%s")
	)
       ("\\`http://kotobank\\.jp/word/"
	dic-lookup-w3m-filter-related-links
	"jj-kotobank" jj "http://kotobank.jp/word/%s")

       ;; 書き順でGO
       ("\\`http://www\\.kkjn\\.jp/"
	(w3m-filter-delete-regions	
	 "<body[^>]*>" "<!-- kaki-res2 -->" t t t)
	)

       ;; 漢字ひつじゅん君
       ("http://www\\.human\\.gr\\.jp/hitsujun/"
	(dic-lookup-w3m-filter-refresh-url
	 "%s"
	 "<td width=\"100%\"><IMG [^>]*src=\"\\([^\"]*\\)[^>]*>"
	 1))

       ;;  "正しい漢字の書き順"
       ("http://kakijun\\.jp/page/"
	(w3m-filter-delete-regions	
	 "<body[^>]*>"
	 "<img src=.* id=\"HJ_0gif\">" t t t t)
	(w3m-filter-replace-regexp
	 "\\(<img src=\\(\"[^\"]*\"\\).* id=\"HJ_0gif\">\\)"
	 "\\1\n<p><a href=\\2>GIF動画</a> M-x image-toggle-animation</p>")
	(dic-lookup-w3m-filter-refresh-url
	 "%s"
	 "<img src=\"\\([^\"]*\\)\".* id=\"HJ_0gif\">" 1)
	)

       ;; gigadict
       ("\\`http://cgi\\.geocities\\.jp/abelinternational/cgi/kanjidic\\.cgi"
	dic-lookup-w3m-filter-related-links "Kanji-gigadict" kanji)
       ("\\`http://cgi\\.geocities\\.jp/abelinternational/cgi/jkdic\\.cgi"
	dic-lookup-w3m-filter-related-links "KKanji-gigadict" kanji)

       ;; FOKS Forgiving Online Kanji Search
       ("\\`http://foks\\.info/search/"
	dic-lookup-w3m-filter-related-links "kanji-foks" kanji)

       ;; kitajiro
       ("\\`http://www\\.ctrans\\.org/search\\.php.*&opts=fw"
	dic-lookup-w3m-filter-related-links "cj-kitajiro" cj)
       ("\\`http://www\\.ctrans\\.org/search\\.php.*&opts=jp"
	dic-lookup-w3m-filter-related-links "jc-kitajiro" cj)
       ("\\`http://www\\.ctrans\\.org/"
	(w3m-filter-delete-regions "<p class=\"edit\">" "</p>")
	(dic-lookup-w3m-filter-word-anchor
	 "pinyin-mandarinspot"
	 "<span class=\"cn\" xml:lang=\"zh\" lang=\"zh\">\\(.*\\)</span>" 1)
	;; (w3m-filter-replace-regexp
	;;  "<span class=\"cn\" xml:lang=\"zh\" lang=\"zh\">\\(.*\\)</span>"
	;;  "・ \\1 ⇒<a href=\"http://mandarinspot.com/annotate?text=\\1&spaces=1&phs=pinyin&show=both\">pinyin</a>")
	(dic-lookup-w3m-filter-conv-pinyin
	 "<span class=\"pyn\">\\(.*\\)</span>")
	)

       ;; weblio
       ("\\`http://thesaurus\\.weblio\\.jp/content/"
	(w3m-filter-delete-regions "<div ID=base>" "<form[^>]*>" nil t nil t)
	(w3m-filter-delete-regions "<div ID=formBoxWrp>" "<div ID=formBoxL>")
	(w3m-filter-delete-regions "<div ID=formBoxR>" "</div>")
	(w3m-filter-delete-regions "</form>" "<div class=kiji>" t t)
	(dic-lookup-w3m-filter-related-links "thesaurus-j-weblio" jj)
	)
       ("\\`http://ejje\\.weblio\\.jp/content/"
	(w3m-filter-delete-regions "<body[^>]*>" "<div ID=topic>" t nil t)
	(w3m-filter-delete-regions
	 "<!-- START Espritline Affiliate CODE -->"
	 "<!-- END Espritline Affiliate CODE -->")
	(w3m-filter-delete-regions "<div class=adBoxHE>" "</body>" nil t)
	;; (w3m-filter-replace-regexp "<span>用例</span>" "[用例]")
	;; (w3m-filter-replace-regexp "<div class=KejjeYrTtl>用例</div>" "[用例]")
	;; (dic-lookup-w3m-filter-convert-phonetic-symbol
	;;  dic-lookup-w3m-filter-weblio-ej-symbol-alist
	;;  "<img [^>]*src=\"http://www\\.weblio\\.jp/[^>]*/\\([^/\" ]+\\)\\.\\(gif\\|png\\)\"[^>]*>")
	(w3m-filter-replace-regexp
	 "<img src=\"http://www.westatic.com/img/showMorePlus.png\"[^>]*>" "")
	(w3m-filter-replace-regexp
	 "<img src=\"http://www.westatic.com/img/icons/iconWlaAdFL.png\"[^>]*>"
	 "")
	(w3m-filter-replace-regexp "<h2>発音記号</h2>" " 発音記号")
	(w3m-filter-replace-regexp "<div class=phoneticEjjeWrp>\\(.*\\)</div>" "\\1")
	(w3m-filter-replace-regexp "<h2 class=audioEjjeTtl>音声を聞く</h2>" "")
	(w3m-filter-replace-regexp
	 "<div [^>]*playSwfSound('http://ejje.westatic.com/audio/', '\\([^']+\\)'[^>]*><img [^>]*></div>"
	 "<a href=\"http://ejje.westatic.com/audio/\\1.wav\">♪再生</a>")
	(w3m-filter-replace-regexp
	 "<div [^>]*playSwfSound('http://www.westatic.com/wbr/CHUJITEN/', '\\([^']+\\)'[^>]*><img [^>]*></div>"
	 "<a href=\"http://www.westatic.com/wbr/CHUJITEN/\\1.wav\">♪再生</a>")
	(w3m-filter-replace-regexp
	 "<td [^>]*><span [^>]*>用例</span></td>"
	 "<td valign=\"top\"><span>[例]</span></td>")
	;;(w3m-filter-replace-regexp "<p class=level0>\\([^<]*\\)</p>" "\\1")
	;;(w3m-filter-replace-regexp "<p class=lvlNH>\\([^<]*\\)</p>" "\\1")
	;;(w3m-filter-replace-regexp "<p class=lvlAH>\\([^<]*\\)</p>" "\\1")
	;;(w3m-filter-replace-regexp "<p class=lvlB>\\([^<]*\\)</p>" "\\1")
	;; (w3m-filter-replace-regexp "<p[^>]*>" " " "<div class=level0>" nil nil nil "div")
	;; (w3m-filter-replace-regexp "</p>" "" "<div class=level0>" nil nil nil "div")
	;; (w3m-filter-replace-regexp "<br[^>]*>" "" "<div class=level0>" nil nil nil "div")
	;; (w3m-filter-replace-regexp "</div>" "" "<div class=level0>" nil nil nil "div")
	;; (w3m-filter-replace-regexp "<div[^>]*>" " " "<div class=level0>" nil nil nil "div")
	(w3m-filter-replace-regexp "<p class=\\(lvlB\\|lvlAH\\|lvlNH\\|level0\\)>" "")
       	(dic-lookup-w3m-filter-related-links "ej-weblio" ej)
	(dic-lookup-w3m-filter-show-candidates "ej-weblio")
	(w3m-filter-delete-regions
	 "<!-- begin ad tag-->" "<!-- End ad tag -->")
	)
       ("\\`http://www\\.weblio\\.jp/content/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div ID=tpc>" t nil t)
	(w3m-filter-delete-regions
	 "<!-- google_ad_section_end -->" "</body>" nil t)
	(w3m-filter-replace-regexp
	 "<img src=\"http://www.westatic.com/img/icons/wRenew/iconPBDict.png\" alt=\"\">" "")
	(w3m-filter-replace-regexp "</div><div style=\"margin-left:1em;\">" "")
	)
       ("\\`http://cjjc\\.weblio\\.jp/content/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div ID=topic>" t nil t)
	(w3m-filter-delete-regions
	 "<td ID=trnsBxHTL>" "</body>" nil t)
       	(dic-lookup-w3m-filter-related-links "cj-weblio" cj)
	)
       ("\\`http://shuwa\\.weblio\\.jp/content/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div ID=topic>" t nil t)
	(w3m-filter-delete-regions
	 "<div id=sideRankBoxS>" "</body>" nil t)
	(w3m-filter-replace-regexp
	 "<object data=\"\\([^\"]+\\)\" type=\"application/x-mplayer2\"[^>]*>"
	 "<a href=\"\\1\">[手話を再生]</a>")
	)

       ;; yahoo encyclopedia
       ("\\`http://100\\.yahoo.co\\.jp/"
	(w3m-filter-delete-regions "<body>" "<!-- /header -->" t)
	(dic-lookup-w3m-filter-related-links "encyclopedia-yahoo" jj)
	)

       ;; dokochina pinyin
       ("\\`http://dokochina\\.com/simplified\\.php"
	(w3m-filter-delete-regions
	 "<body[^>]*>"
	 "<DIV STYLE='overflow-x:scroll; width:600px'><table border=0 cellspacing=0 cellpadding=0 bgcolor=#FFFFFF>" t t t)
	(w3m-filter-delete-regions "<!--**********-->" "</body>" nil t)
	)

       ;; 書虫 pinyin
       ("http://www\\.frelax\\.com/cgi-local/pinyin/hz2py\\.cgi"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "</form>" t nil t)
	(w3m-filter-replace-regexp
	 "<table border=\"4\" cellpadding=\"2\" cellspacing=\"0\" bordercolor=\"#996633\">"
	 "<table border=\"0\">")
	(w3m-filter-replace-regexp
	 "<td *align=\"center\">" "<td align=\"left\">")
	(w3m-filter-replace-regexp
	 "</?center>" "")
	)

       ;; pinyin chinese1
       ("\\`http://www\\.chinese1\\.jp/pinyin/gb2312/jp\\.asp"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<table border=\"0\" cellpadding=\"0\" cellspacing=\"10\">" t t t)
	(w3m-filter-delete-regions
	 "^<div align=\"right\">" "</body>" nil t t)
	(dic-lookup-w3m-filter-related-links "pinyin-chinese1" pinyin)
	(w3m-filter-replace-regexp
	 "<input type=\"submit\" value=\"S\" name=\"S\" style=\"width: 15; height: 20\">" "")
	)

       ;; pinyin cazoo
       ("http://www\\.cazoo\\.jp/cgi-bin/pinyin/index\\.html\\?hanzi="
	dic-lookup-w3m-filter-related-links "pinyin-cazoo" pinyin
	nil nil "</head>")

       ;; goo
       ("\\`http://dictionary\\.goo\\.ne\\.jp/srch/"
	dic-lookup-w3m-filter-goo-jump-to-1stcontent
	"http://dictionary.goo.ne.jp%s"
	"<a href=\"\\(/leaf/.*/m0u/[^/]*/\\)" 1)
       ("\\`http://dictionary\\.goo\\.ne\\.jp/"
	(w3m-filter-delete-regions "<body[^>]*>" "<div class=\"wordTitle\">" t t t)
	(w3m-filter-delete-regions "<body[^>]*>" "<dl class=\"allList\">" t t t)
	(w3m-filter-delete-regions "<body[^>]*>" "^<!-- inner tab -->" t nil t t)
	(w3m-filter-delete-regions "<!--c34-->" "</body>" nil t)
	(w3m-filter-delete-regions "<!--/result-->" "</body>" nil t)
	(dic-lookup-w3m-filter-eword-anchor "ej-goo")
	(dic-lookup-w3m-filter-convert-phonetic-symbol
	 dic-lookup-w3m-filter-ocn-ej-symbol-alist
	 "<img src=\"[^>]*/img[^>]*/\\([a-z_0-9]+\\)\\.gif\"[^>]*>")
	(w3m-filter-delete-regions "<div class=\"buttons-panel\">"
				   "</div>" nil t)
	(w3m-filter-delete-regions "<ul class=\"enditMean\">" "</ul>")
	(w3m-filter-replace-regexp "\\(<div id=\"spoLine\">\\)" "<br>\\1")
	)
       ("\\`http://dictionary\\.goo\\.ne\\.jp/srch/ej/"
	dic-lookup-w3m-filter-related-links "ej-goo" ej)
       ("\\`http://dictionary\\.goo\\.ne\\.jp/leaf/ej3/"
	dic-lookup-w3m-filter-related-links "ej-goo" ej "/m0u/%s/")
       ("\\`http://dictionary\\.goo\\.ne\\.jp/srch/je/"
	dic-lookup-w3m-filter-related-links "je-goo" ej)
       ("\\`http://dictionary\\.goo\\.ne\\.jp/leaf/je2/"
	dic-lookup-w3m-filter-related-links "je-goo" ej "/m0u/%s/")
       ("\\`http://dictionary\\.goo\\.ne\\.jp/srch/jn/"
	dic-lookup-w3m-filter-related-links "jj-goo" jj)
       ("\\`http://dictionary\\.goo\\.ne\\.jp/leaf/jn2/"
	dic-lookup-w3m-filter-related-links "jj-goo" jj "/m0u/%s/")
       ("\\`http://dictionary\\.goo\\.ne\\.jp/srch/cj/"
	dic-lookup-w3m-filter-related-links "cj-goo" cj)
       ("\\`http://dictionary\\.goo\\.ne\\.jp/leaf/cj/"
	(dic-lookup-w3m-filter-related-links "cj-goo" cj "/m0u/%s/")
	(w3m-filter-replace-regexp
	 "\\(<span class=\"ex\">[^<]*</span>\\)" "\\1 - ")
	(w3m-filter-replace-regexp
	 "\\(<span class=\"pinyin\">\\)" " \\1")
	(dic-lookup-w3m-filter-word-anchor
	 "pinyin-mandarinspot" "<span class=\"ex\">\\([^<]*\\)</span>" 1)
	(w3m-filter-replace-regexp
	 "【同】\\([^<)]+\\)\\([^<]*\\)</span>"
	 "【同】<a href=\"/srch/cj/\\1/m0u/\">\\1</a>\\2</span>")
	)
       ("\\`http://dictionary\\.goo\\.ne\\.jp/srch/jc/"
	dic-lookup-w3m-filter-related-links "jc-goo" cj)
       ("\\`http://dictionary\\.goo\\.ne\\.jp/leaf/jc/"
	(dic-lookup-w3m-filter-related-links "jc-goo" cj "/m0u/%s/")
	(w3m-filter-replace-regexp
	 "<span class=\"btn_sound\">\\([^>]*>\\)<img src=\"/img/btn_sound.gif\"></a></span>"
	 "\\1→中日</a>")
	(w3m-filter-replace-regexp
	 "\\(<div class=\"prog_example\">\\|<br />\\)\\([^>]*\\)\\(　<span class=\"pinyin\">\\)"
	 "\\1<a href=\"/srch/cj/\\2/m0u/\">\\2</a>\\3")
	)
       ("\\`http://dictionary\\.goo\\.ne\\.jp/"
	dic-lookup-w3m-filter-show-candidates "ej-goo")

       ;; ocn goo
       ("\\`http://ocndictionary\\.goo\\.ne\\.jp/search\\.php"
	(w3m-filter-delete-regions "<body[^>]*>" "<dl class=\"allList\">" t nil t t)
	(w3m-filter-delete-regions "<!--l14_4-->\r" "<!--/result-->")
	(w3m-filter-delete-regions "<!--/rbox-->" "</body>" nil t)
	(w3m-filter-delete-regions "<div id=\"rside\">" "</body>" nil t)
	(dic-lookup-w3m-filter-eword-anchor "ej-ocn")
	(dic-lookup-w3m-filter-convert-phonetic-symbol
	 dic-lookup-w3m-filter-ocn-ej-symbol-alist
	 "<img src=\"[^>]*/img[^>]*/\\([a-z_0-9]+\\)\\.gif\"[^>]*>")
	)
       ("\\`http://ocndictionary\\.goo\\.ne\\.jp/search\\.php.*kind=\\(ej\\|je\\)"
	dic-lookup-w3m-filter-related-links "ej-ocn" ej)
       ("\\`http://ocndictionary\\.goo\\.ne\\.jp/search\\.php.*kind=jn"
	dic-lookup-w3m-filter-related-links "jj-ocn" jj)
       ("\\`http://ocndictionary\\.goo\\.ne\\.jp/search\\.php"
	dic-lookup-w3m-filter-show-candidates "ej-ocn")

       ;; NAVER 韓日、日韓
       ("\\`http://krdic\\.naver\\.jp/\\(search\\|entry\\)/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div class=\"[^\"]*section3" t t t t)
	(w3m-filter-delete-regions
	 "韓国語 動詞,形容詞活用情報</a>" "</body>" t t)
	(w3m-filter-replace-regexp
	 "\\(var g_query = \"\\([^\"]*\\)\";\\(?:\n.*\\)*\\)<a href=[^>]*>例文もっと見る</a>"
	 "\\1<a href=\"/search/ex/1/\\2\">例文もっと見る</a>")
	(w3m-filter-replace-regexp
	 "\\(var g_query = \"\\([^\"]*\\)\";\\(?:\n.*\\)*\\)<a href=[^>]*getParams('example', \\([0-9]*\\)[^>]*>前ページ</a>"
	 "\\1<a href=\"/search/ex/\\3/\\2\">前ページ</a>")
	(w3m-filter-replace-regexp
	 "\\(var g_query = \"\\([^\"]*\\)\";\\(?:\n.*\\)*\\)<a href=[^>]*getParams('example', \\([0-9]*\\)[^>]*>次ページ</a>"
	 "\\1<a href=\"/search/ex/\\3/\\2\">次ページ</a>")
	)
       ("\\`http://krdic\\.naver\\.jp/entry/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div class=\"spot_area\">" t t t t)
	(w3m-filter-delete-regions
	 "<div class=\"list_select\">" "<div class=\"section\">" nil t)
	)
       ;; NAVER 中日、日中
       ("http://cndic\\.naver\\.jp/\\(srch\\|cje\\|jce\\)/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div class=\"word_view\">" t t t)
	(w3m-filter-delete-regions
	 "<div class=\"pron\">" "<div class=\"into\">" nil t)
	(w3m-filter-replace-regexp
	 "<a href=[^>]*>中日もっと見る</a>\\(\\(\n\\|[^\n]\\)*var g_query = \"\\([^\"]*\\)\";\\)"
	 "<a href=\"/srch/cj/1/\\3\">中日もっと見る</a>\\1")
	(w3m-filter-replace-regexp
	 "<a title=\"前ページ\"[^>]*getParams('cjentry', \\([0-9]*\\)[^>]*>.*</a>\\(\\(?:\n\\|[^\n]\\)*var g_query = \"\\([^\"]*\\)\";\\)"
	 "<a href=\"/srch/cj/\\1/\\3\">前ページ</a>\\2")
	(w3m-filter-replace-regexp
	 "<a title=\"次ページ\"[^>]*getParams('cjentry', \\([0-9]*\\)[^>]*>.*</a>\\(\\(?:\n\\|[^\n]\\)*var g_query = \"\\([^\"]*\\)\";\\)"
	 "<a href=\"/srch/cj/\\1/\\3\">次ページ</a>\\2")
	(w3m-filter-replace-regexp
	 "<a href=[^>]*>例文もっと見る</a>\\(\\(\n\\|[^\n]\\)*var g_query = \"\\([^\"]*\\)\";\\)"
	 "<a href=\"/srch/ex/1/\\3\">例文もっと見る</a>\\1")
	(w3m-filter-replace-regexp
	 "<a title=\"前ページ\"[^>]*getParams('example', \\([0-9]*\\)[^>]*>.*</a>\\(\\(?:\n\\|[^\n]\\)*var g_query = \"\\([^\"]*\\)\";\\)"
	 "<a href=\"/srch/ex/\\1/\\3\">前ページ</a>\\2")
	(w3m-filter-replace-regexp
	 "<a title=\"次ページ\"[^>]*getParams('example', \\([0-9]*\\)[^>]*>.*</a>\\(\\(?:\n\\|[^\n]\\)*var g_query = \"\\([^\"]*\\)\";\\)"
	 "<a href=\"/srch/ex/\\1/\\3\">次ページ</a>\\2")
	(w3m-filter-delete-regions
	 "<div style=\"top: 413px; left: 90px; display:none\" class=\"ly_play example_play\" id=\"div_exmple_pingyin\">"
	 "<!-- //CONTENT -->")
	(w3m-filter-replace-regexp
	 "<a [^>]*pUrl=\"\\([^|]+\\)|\\([^|]+\\)[^>]*><img [^>]*></a>"
	 " ♪<a href=\"\\1\" type=\"audio/mpeg\">女性</a>|<a href=\"\\2\" type=\"audio/x-wav\">男性</a>")
	(w3m-filter-replace-regexp
	 "<a [^>]*><img [^>]*pUrl=\"\\([^|]+\\)|\\([^|]+\\)[^>]*></a>"
	 " ♪<a href=\"\\1\" type=\"audio/mpeg\">女性</a>|<a href=\"\\2\" type=\"audio/x-wav\">男性</a>")
	(w3m-filter-delete-regions
	 "<a href=\"#\" class=\"x\" title=\"削除\"" "</a>")
	(w3m-filter-delete-regions
	 "<a href=\"#\" alt=\"クリア\" title=\"クリア\" class=\"btn_delete"
	 "</a>")
	)
       ("http://cndic\\.naver\\.jp/srch/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div class=\"zoom_lv\" id=\"zoom\">" t t t t)
	)
       ("http://cndic.naver.jp/cje/"
	(w3m-filter-delete-regions
	 "<div class=\"pron\">" "<div class=\"section\">")
	(w3m-filter-delete-regions
	 "<div style=\"visibility: hidden; left: 15px; top: 114px;\" class=\"controller control0\">"
	 "</body>" nil t)
	(w3m-filter-replace-regexp
	 "<a [^>]*strokeOrdFile=\"\\([^\"]+\\)\"[^>]*>書き順を表示[^<]*</a>"
	 "<a href=\"http://dicimg.naver.com/cndic/chinese/stroke/\\1\" type=\"application/x-shockwave-flash\">書き順</a>")
	(w3m-filter-replace-regexp
	 "<span class=\"eword\">" "<br><span class=\"eword\">")
	(w3m-filter-replace-regexp
	 "<a href=\"#\" class=\"play\"[^>]*><img [^>]*clickcr(this,'pos.examlisten'[^>]*></a>"
	 "&nbsp;")
	(w3m-filter-delete-regions "<button class=\"repeat\"" "</button>")
	(w3m-filter-delete-regions "<button class=\"speed\"" "</button>")
	)

       ;; Glosbe 多言語オンライン辞書、翻訳メモリ
       ("\\`http://\\([a-z]+\.\\)?glosbe\.com/[^/]+/[^/]+/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div [^>]*id=\"wordListWidget\">" t t t t)
	(w3m-filter-delete-regions "<div id=\"footer\">" "</body>" nil t)
	(w3m-filter-replace-regexp
	 "<a [^>]*class=\"meaningLink\"[^<]*><img [^>]*src=\"http://glosbe.com/resources/resources_img/edit.png\"/></a>"
	 "")
	(w3m-filter-replace-regexp
	 "<a [^>]*class=\"[^\"]*audio[^>]*data-url=\\(\"[^\"]+\"\\)[^>]*><img [^>]*/></a>"
	 " <a href=\\1\">♪</a>")
	(w3m-filter-delete-regions
	 "<aside><div authorUrl=[^>]*>" "</div></aside>" nil nil t)
	)

       ;;
       ;; translators
       ;;

       ;; yahoo translator
       ("\\`http://honyaku\\.yahoo\\.co\\.jp/transtext"
	(w3m-filter-delete-regions 
	 "<body>" "<textarea [^>]*id=\"textText\"" t t nil t)
	(w3m-filter-delete-regions
	 "</textarea>" "<textarea [^>]*id=\"trn_textText\"" t t nil t)
	(w3m-filter-replace-regexp
	 "<textarea [^>]*id=\"textText\"[^>]*>\\([^<]*\\)</textarea>"
	 "<p>\\1</p>")
	(w3m-filter-replace-regexp
	 "<textarea [^>]*id=\"trn_textText\"[^>]*>\\([^<]*\\)</textarea>"
	 "<hr><p>\\1</p>")
	(w3m-filter-delete-regions "<wbr" ">")
	(dic-lookup-w3m-filter-eword-anchor dic-lookup-w3m-favorite-ej-engine)
	(w3m-filter-replace-regexp "\n" "<br>\n")
	(w3m-filter-delete-regions "</div><!-- /#transafter -->" "</body>"
				   nil t)
	)

       ;; excite translator
       ("\\`http://www\\.excite\\.co\\.jp/world/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<textarea [^>]*name=\"before\"[^>]*>" t nil t t)
	(w3m-filter-replace-regexp
	 "\\(<textarea [^>]*name=\"after\"[^>]*>\\)" "\\1<br>")
	(w3m-filter-delete-regions
	 "</textarea>" "<textarea [^>]*name=\"after\"[^>]*>" nil nil t t)
	(w3m-filter-delete-regions "</textarea>" "</body>" nil t)
	(w3m-filter-replace-regexp "\r\\([\n]\r\\)+" "</p><p>")
	(w3m-filter-replace-regexp "\r" "<br>")
	)
       ("\\`http://www\\.excite\\.co\\.jp/world/english/"
	dic-lookup-w3m-filter-eword-anchor "ej-excite")

       ;; google translator
       ("https://translate\\.google\\.com"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<textarea [^>]*id=source[^>]*>" t nil t t)
	(w3m-filter-delete-regions
	 "</textarea>" "<span id=result_box[^>]*>" t t nil t)
	(w3m-filter-replace-regexp "\\(</textarea>\\)" "<hr>")
	(w3m-filter-replace-regexp "<span title=\"[^\"]*\"[^>]*>" "<span>")
	(w3m-filter-replace-regexp "\r\\([\n]\r\\)+" "</p><p>")
	(w3m-filter-delete-regions "<div id=\"gt-edit\"" "</body>" nil t)
	(w3m-filter-delete-regions "<div id=res-translit" "</body>" nil t)
	(dic-lookup-w3m-filter-eword-anchor dic-lookup-w3m-favorite-ej-engine)
	)

       ;; yahoo.com translator
       ("http://babelfish\\.yahoo\\.com/translate_txt"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<div id=\"result\">" t t t)
	(w3m-filter-replace-regexp
	 "\\(<textarea [^>]*name=\"trtext\"[^>]*>\\)" "\\1<hr>")
	(w3m-filter-delete-regions
	 "</div></div>" "<textarea [^>]*name=\"trtext\"[^>]*>" nil nil t t)
	(w3m-filter-delete-regions "</textarea>" "</body>" nil t)
	(w3m-filter-replace-regexp "\\([^\r]\\)$" "\\1<br>")
	(w3m-filter-replace-regexp "\r\\([\n]\r\\)+" "<p>")
	(w3m-filter-replace-regexp "\r" "<br>")
	(dic-lookup-w3m-filter-eword-anchor dic-lookup-w3m-favorite-ej-engine)
	)

       ;; freetranslation translator
       ("\\`http://.*\\.freetranslation\\.com"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<textarea [^>]*name=\"dsttext\"[^>]*>" t nil t t)
	(w3m-filter-replace-regexp
	 "\\(<textarea [^>]*name=\"srctext\"[^>]*>\\)" "\\1<hr>")
	(w3m-filter-delete-regions
	 "</textarea>" "<textarea [^>]*name=\"srctext\"[^>]*>" nil nil t t)
	(w3m-filter-delete-regions "</textarea>" "</body>" nil t)
	(w3m-filter-replace-regexp "\r\\([\n]\r\\)+" "</p><p>")
	(w3m-filter-replace-regexp "\r" "<br>")
	)
       ("\\`http://tets9\\.freetranslation\\.com/"
	dic-lookup-w3m-filter-eword-anchor dic-lookup-w3m-favorite-ej-engine)

       ;; ;; ocn translator サービス終了 2012年10月11日
       ;; ("\\`http://cgi01\\.ocn\\.ne\\.jp/cgi-bin/translation/index\\.cgi"
       ;; 	(w3m-filter-delete-regions
       ;; 	 "<body[^>]*>" "<textarea [^>]*name=\"sourceText\"[^>]*>" t nil t t)
       ;; 	(w3m-filter-replace-regexp
       ;; 	 "\\(<textarea [^>]*name=\"responseText\"[^>]*>\\)" "\\1<hr>")
       ;; 	(w3m-filter-delete-regions
       ;; 	 "</textarea>" "<textarea [^>]*name=\"responseText\"[^>]*>" nil nil t t)
       ;; 	(w3m-filter-delete-regions "</textarea>" "</body>" nil t)
       ;; 	(w3m-filter-replace-regexp "\r\\([\n]\r\\)+" "</p><p>")
       ;; 	(w3m-filter-replace-regexp "\r" "<br>")
       ;; 	(dic-lookup-w3m-filter-eword-anchor "ej-ocn")
       ;; 	)

       ;; livedoor translator
       ("http://livedoor-translate\\.naver\\.jp/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<textarea [^>]*name=\"translateParams.originalText\"[^>]*>" t nil t t)
	(w3m-filter-replace-regexp
	 "\\(<textarea [^>]*name=\"text02\"[^>]*>\\)" "\\1<hr>")
	(w3m-filter-delete-regions
	 "</textarea>" "<textarea [^>]*name=\"text02\"[^>]*>" nil nil t t)
	(w3m-filter-delete-regions "</textarea>" "</body>" nil t)
	(w3m-filter-delete-regions "<!--/MdAd01-->" "</html>")
	(w3m-filter-delete-regions "<!--/MdAd02-->" "</html>")
	(w3m-filter-replace-regexp "\\(\\. \\|。\\)" "\\1<br>")
	)
       ("\\`http://livedoor-translate\\.naver\\.jp/"
	dic-lookup-w3m-filter-eword-anchor dic-lookup-w3m-favorite-ej-engine)

       ;; fresheye translator
       ("\\`http://mt\\.fresheye\\.com/ft_.*result.cgi"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<textarea [^>]*name=\"gen_text\"[^>]*>" t nil t t)
	(w3m-filter-replace-regexp
	 "\\(<textarea [^>]*name=\"gen_text2\"[^>]*>\\)" "\\1<hr>")
	(w3m-filter-delete-regions
	 "</textarea>" "<textarea [^>]*name=\"gen_text2\"[^>]*>" nil nil t t)
	(w3m-filter-delete-regions "</textarea>" "</body>" nil t nil t)
	(w3m-filter-replace-regexp "\n" "<br>")
	(dic-lookup-w3m-filter-eword-anchor dic-lookup-w3m-favorite-ej-engine)
	)

       ;; So-net translator
       ("\\`http://so-net\\.web\\.transer\\.com/text_trans_sn\\.php"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<textarea [^>]*name=\"text\"[^>]*>" t nil t t)
	(w3m-filter-replace-regexp
	 "\\(<textarea [^>]*name=\"translatedText\"[^>]*>\\)" "\\1<hr>")
	(w3m-filter-delete-regions
	 "</textarea>"
	 "<textarea [^>]*name=\"translatedText\"[^>]*>" nil nil t t)
	(w3m-filter-delete-regions "</textarea>" "</body>" nil t)
	(w3m-filter-replace-regexp "\n" "<br>")
	(dic-lookup-w3m-filter-eword-anchor dic-lookup-w3m-favorite-ej-engine)
	)

       ;; nifty translator
       ("\\`http://honyaku-result\\.nifty\\.com/LUCNIFTY/text/text.php"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<textarea [^>]*id=\"txtTransArea\"[^>]*>" t nil t t)
	(w3m-filter-replace-regexp
	 "\\(<textarea [^>]*id=\"txtTransArea2\"[^>]*>\\)" "\\1<hr>")
	(w3m-filter-delete-regions
	 "</textarea>"
	 "<textarea [^>]*id=\"txtTransArea2\"[^>]*>" nil nil t t)
	(w3m-filter-delete-regions "</textarea>" "</body>" nil t)
	(w3m-filter-replace-regexp "\n" "<br>")
	(dic-lookup-w3m-filter-eword-anchor dic-lookup-w3m-favorite-ej-engine)
	)

       ;; magicalgate translator
       ;; http://ag.magicalgate.net
       ("http://221\\.243\\.5\\.2/impulse/TextTranslator"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<textarea [^>]*name=\"srcText\"[^>]*>" t nil t t)
	(w3m-filter-replace-regexp
	 "\\(<textarea[^>]*name=\"resultText\"[^>]*>\\)" "\\1<hr>")
	(w3m-filter-delete-regions
	 "</textarea>"
	 "<textarea[^>]*name=\"resultText\"[^>]*>" nil nil t t)
	(w3m-filter-delete-regions "</textarea>" "</body>" nil t)
	(w3m-filter-replace-regexp "\n" "<br>")
	(dic-lookup-w3m-filter-eword-anchor dic-lookup-w3m-favorite-ej-engine)
	)

       ;; infoseek translator
       ("\\`http://translation\\.infoseek\\.co\\\.jp/"
	(w3m-filter-delete-regions
	 "<body[^>]*>" "<textarea [^>]*name=\"original\"[^>]*>" t nil t t)
	(w3m-filter-replace-regexp
	 "\\(<textarea [^>]*name=\"converted\"[^>]*>\\)" "\\1<hr>")
	(w3m-filter-delete-regions
	 "</textarea>"
	 "<textarea [^>]*name=\"converted\"[^>]*>" nil nil t t)
	(w3m-filter-delete-regions "</textarea>" "</body>" nil t)
	(w3m-filter-replace-regexp "\n" "<br>")
	(dic-lookup-w3m-filter-eword-anchor dic-lookup-w3m-favorite-ej-engine)
	)
       ))))

(defvar dic-lookup-w3m-inline-image-rules '())

(mapc
 #'(lambda (elem) (add-to-list 'dic-lookup-w3m-inline-image-rules elem))
 '(("\\`http://www\\.excite\\.co\\.jp/dictionary/english_japanese/\\?search=" . t)
   ("\\`http://www\\.excite\\.co\\.jp/dictionary/japanese/\\?search=" . t)
   ("\\`http://www\\.excite\\.co\\.jp/dictionary/chinese_japanese/\\?search=" . t)
   ("\\`http://www\\.excite\\.co\\.jp/dictionary/japanese_chinese/\\?search=" . t)
   ("\\`http://dic\\.search\\.yahoo\\.co\\.jp/dsearch\\?" . t)
   ("\\`http://eow\\.alc\\.co\\.jp/.*/UTF-8/" . turnoff)
   ("\\`http://home\\.alc\\.co\\.jp/db/owa/bdicn_sch" . turnoff)
   ("\\`http://www\\.merriam-webster\\.com/dictionary/" . turnoff)
   ("\\`http://dictionary\\.cambridge\\.org/results.asp\\?searchword=" . turnoff)
   ("\\`http://sara\\.natcorp\\.ox\\.ac\\.uk/cgi-bin/saraWeb\\?qy=" . turnoff)
   ("\\`http://www\\.collins\\.co\\.uk/Corpus/CorpusPopUp\\.aspx\\?query=" . turnoff)
   ("\\`http://erek\\.ta2o\\.net/" . turnoff)
   ("\\`http://www\\.kotonoha\\.gr\\.jp/demo/search_result\\?query_string=" . turnoff)
   ;;("\\`http://www\\.kotonoha\\.gr\\.jp/demo" . turnoff)
   ("\\`http://www\\.excite\\.co\\.jp/world/" . turnoff)
   ("\\`http://honyaku\\.yahoo\\.co\\.jp/transtext" . turnoff)
   ("\\`http://translate\\.google\\.com/translate_t" . turnoff)
   ("\\`http://babelfish\\.yahoo\\.com/translate_txt" . turnoff)
   ("\\`http://tets9\\.freetranslation\\.com" . turnoff)
   ("\\`http://.*\\.weblio\\.jp/" . turnoff)
   ("\\`http://thesaurus\\.reference\\.com/browse/" . turnoff)
   ("\\`http://lsd-project\\.jp/weblsd/" . t)
   ("\\`http://lsd\\.pharm\\.kyoto-u\\.ac\\.jp/cgi-bin/lsdproj/etoj-cgi04\\.pl" . turnoff)
   ("\\`http://www\\.ctrans\\.org/cjdic/search\\.php" . turnoff)
   ("\\`http://bitex-cn\\.com/search_result\\.php" . turnoff)
   ("\\`http://cgi\\.geocities\\.jp/abelinternational/cgi/diccj\\.cgi" . turnoff)
   ("\\`http://dict\\.hjenglish\\.com/jp/w/" . turnoff)
   ("\\`http://dictionary\\.infoseek\\.ne\\.jp/word" . t)
   ("\\`http://dictionary\\.infoseek\\.ne\\.jp/.+word" . turnoff)
   ("\\`http://kotobank\\.jp/word" . t)
   ("\\`http://www\\.onlinedic\\.com/search\\.php" . turnoff)
   ("\\`http://www\\.frelax\\.com/cgi-local/pinyin/hz2py\\.cgi" . turnoff)
   ("\\`http://www\\.babylon\\.com/definition/" . turnoff)
   ("\\`http://100\\.yahoo\\.co\\.jp/" . turnoff)
   ("\\`http://ext\\.dictionary\\.goo\\.ne\\.jp/" . t)
   ("\\`http://ocndictionary\\.goo\\.ne\\.jp/search\\.php" . t)
   ("\\`http://www5\\.mediagalaxy\\.co\\.jp/CGI/sanshushadj/search\\.cgi" . t)
   ;; ("\\`http://ejje\\.weblio\\.jp/content/" . t)
   ("\\`http://www\\.kkjn\\.jp/" t)
   ("\\`http://www\\.human\\.gr\\.jp/hitsujun/" t)
   ("\\`http://kakijun\\.jp/page/" t)
   ("\\`http://education\\.yahoo\\.com/reference/[^/]+/" t)
   ))

(defvar dic-lookup-w3m-related-site-list '())
(add-to-list
 'dic-lookup-w3m-related-site-list
 '(ej
   (("ej-excite" . "excite")
    ("ej-weblio" . "weblio")
    ("ej-yahoo" . "Y!")
    ("ej-alc" . "alc")
    ("ej-infoseek" . "infoseek")
    ("ej-goo" . "goo")
    ("ej-ocn" . "ocn")
    ("ee-webster" . "webster")
    ("corpus-erek" . "コパerek")
    ("corpus-bnc" . "コパbnc")
    ("thesaurus-webster" . "シソwebster")
    ("thesaurus-rogets" . "シソrogets")
    ("ej-jijieigo" . "時事")
    ;;("thesaurus-j-yahoo" . "J類語")
    ("thesaurus-j-weblio" . "Jシソ")
    ("corpus-j-kotonoha" . "Jコパ")
    ("jj-yahoo" . "国語"))))

(add-to-list
 'dic-lookup-w3m-related-site-list
 '(ee
   (("ee-webster" . "webster")
    ("ee-cambridge" . "cambridge")
    ("ee-longman" . "longman")
    ("ee-oxford" . "oxford")
    ("ee-macmillan" . "macmillan")
    ("ee-onelook" . "onelook")
    ("ee-dict.org" . "dict.org")
    ("ee-yahoo.com" . "yahoo")
    ("ee-dictionrary.com" . "dictionrary.com")
    ("ee-babylon" . "babylon")
    ("ee-wiktionary" . "wiktionary")
    ("ej-yahoo" . "EJ-Y!")
    ("ej-excite" . "EJ-excite")
    )))

(add-to-list
 'dic-lookup-w3m-related-site-list
 '(jj
   (("jj-excite" . "国excite")
    ("jj-yahoo" . "国Y!")
    ("jj-goo" . "国goo")
    ("jj-weblio" . "国weblio")
    ("jj-chuuta" . "国チュウ")
    ("jj-kotobank" . "kotobank")
    ;;("kanji-infoseek" . "漢")
    ;;("jj-katakana-infoseek" . "カタカナ")
    ("jj-yojijukugo-goo" . "四熟")
    ;;("thesaurus-j-yahoo" . "類語Y!")
    ("thesaurus-j-weblio" . "類語weblio")
    ("thesaurus-j-goo" . "類語goo")
    ("kanji-kakijun-kakijun.jp" . "筆")
    ("kanji-kakijun-alphainc" . "順")
    ("kanji-kakijun-hitsujunkun" . "順")
    ("corpus-j-kotonoha" . "Jコパ")
    ("corpus-j-caseframe-get" . "格")
    ("etc-yahoo" . "百科")
    ("ja.wikipedia" . "Wikipedia")
    ("jj-wiktionary" . "Wiktionary")
    ("ej-excite" . "JE-excite")
    ("je-yahoo" . "JE-Y!")
    ("jc-excite" . "中")
    ("JK-gigadict" . "韓"))))

(add-to-list
 'dic-lookup-w3m-related-site-list
 '(kanji
   (;;("kanji-infoseek" . "漢infoseek")
    ("Kanji-gigadict" . "漢gigadict")
    ("KKanji-gigadict" . "教育漢字gigadict")
    ("kanji-foks" . "漢foks")
    ("jj-excite" . "国excite")
    ("cj-excite" . "中日")
    ("jc-excite" . "日中"))))

(add-to-list
 'dic-lookup-w3m-related-site-list
 '(cj
   (("cj-excite" . "CJ-excite")
    ("cj-kitajiro" . "CJ北")
    ("cj-goo" . "CJ-goo")
    ("cj-naver" . "CJ-naver")
    ("cj-bitex" . "CJ-bitex")
    ("cj-tonko-jikai" . "CJ敦煌")
    ("cj-jcdic" . "CJ-jcdic")
    ("cj-weblio" . "CJJC-weblio")
    ("cj-hjenglish" . "CJ-hjenglish")
    ("jc-excite" . "JC-excite")
    ("jc-kitajiro" . "JC北")
    ("jc-bitex" . "JC-bitex")
    ("jc-tonko-jikai" . "JC敦煌")
    ("jc-jcdic" . "JC-jcdic")
    ("jc-goo" . "JC-goo")
    ("jc-hjenglish" . "JC-hjenglish")
    ("pinyin-mandarinspot" . "py-mandarin")
    ("pinyin-cazoo" . "py-cazoo")
    ("jj-yahoo" . "国語"))))

(add-to-list
 'dic-lookup-w3m-related-site-list
 '(pinyin
   (("pinyin-mandarinspot" . "py-mandarin")
    ("pinyin-cazoo" . "py-cazoo")
    ("pinyin-chinese1" . "py-chinese1")
    ("pinyin-frelax" . "py-frelax")
    ("pinyin-ctrans" . "py-ctrans")
    ("pinyin-dokochina" . "py-dokochina")
    ("pinyin-seikei" . "py-seikei")
    ("cj-excite" . "中日")
    ("jc-excite" . "日中"))))

(defvar dic-lookup-w3m-suitable-engine-pattern
  '("[^\000-\177]" "\\(^\\|-\\)\\(ej-\\)" "\\1je-")
  "*検索文字列によって辞書を自動的に切り替えるための規則。
例えば英和辞典で日本語文字列を検索しようとした場合に和英辞典に切り替えて
検索する。`dic-lookup-w3m-suitable-engine'で使用。")

(defvar dic-lookup-w3m-filter-excite-always-show-first-entry t
  "*excite辞書で最初の見出し語の内容を表示する。
exciteの辞書検索で複数の見出し語が見つかった場合でも、最初の見出し語の
内容を表示する。")

(defun dic-lookup-w3m-filter-excite-jump-to-1stcontent
  (url new-url &optional regexp subexp)
  "検索結果の最初の見出し語の説明のページに移動する。"
  (goto-char (point-min))
  (if (or (and dic-lookup-w3m-filter-excite-always-show-first-entry
	       (re-search-forward
		"<span class=\"hSide\"> *\\[1 〜 .*件中\\]</span>" nil t))
	  (re-search-forward
	   "<span class=\"hSide\"> *\\[1 〜 1 / 1件中\\]</span>" nil t))
      (dic-lookup-w3m-filter-refresh-url url new-url regexp subexp)))

(defvar dic-lookup-w3m-filter-goo-always-show-first-entry t
  "*goo辞書で最初の見出し語の内容を表示する。
gooの辞書検索で複数の見出し語が見つかった場合でも、最初の見出し語の
内容を表示する。")

(defun dic-lookup-w3m-filter-goo-jump-to-1stcontent
  (url new-url &optional regexp subexp)
  "検索結果の最初の見出し語の説明のページに移動する。"
  (goto-char (point-min))
  (if dic-lookup-w3m-filter-goo-always-show-first-entry
      (dic-lookup-w3m-filter-refresh-url url new-url regexp subexp)))

(defvar dic-lookup-w3m-filter-excite-ej-symbol-alist
  '(
    ("a121" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078134&offset=0522&frommenu=true\">&#x2020;</a> ") ; ダガー
    ("a122" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078134&offset=0522&frommenu=true\">&#x2021;</a> ") ; ダブルダガー
    ("a123" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078134&offset=0522&frommenu=true\">&#x2021;&#x2021;</a> ") ; ダブルダガー x2
    ("a124" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078134&offset=0522&frommenu=true\">&#x2DA;</a>")	; 上丸
    ("a125" . "&#x306;")		; ブリーヴ(短音符)
    ("a126" . "≪")
    ("a127" . "≫")
    ("a128" . "<strong>〔</strong>")
    ("a129" . "<strong>〕</strong>")
    ("a12a" . "~")
    ("a12b" . "&#x2013; ")		; -
    ("a12c" . "&#x2013;&#x301; ")	; -'
    ("a12d" . "&#x2013;&#x300; ")	; -`
    ("a12e" . "&#x101;&#x301;")		; a-'
    ("a12f" . "&#x113;&#x301;")		; e-'
    ("a130" . "&#x12B;&#x301;")		; i-'
    ("a131" . "&#x361;")		; 上部連結線
    ("a132" . "&#x306;")		; ブリーヴ(短音符)
    ("a133" . "&#x0B8;")		; セディーユ
    ("a134" . "&#x0E7;")		; セディーユ付きのC
    ("a135" . "&#x259;&#x301;")		; シュワー'
    ("a136" . "&#x25A;&#x301;")		; 右鉤付きのシュワー'
    ("a137" . "&#x0CD;")  		; I'
    ("a138" . "&#x254;&#x301;")		; 開いたO'
    ("a139" . "&#x28A;&#x301;")		; ユプシロン'
    ("a13a" . "&#x251;&#x301;")		; 筆記体のA'
    ("a13b" . "&#x301;")		; アポストロフィー'
    ("a13c" . "&#x0C9;")		; E'
    ("a13d" . "&#x0E1;")		; a'
    ("a13e" . "&#x0E9;")  		; e'
    ("a13f" . "&#x0ED;")  		; i'
    ("a140" . "&#x0F3;")  		; o'
    ("a141" . "&#x0FA;")  		; u'
    ("a142" . "&#x28C;&#x301;")		; 逆さのV'
    ("a143" . "&#x259;&#x300;")		; シュワー`
    ("a144" . "&#x25A;&#x300;")		; 右鉤付きのシュワー`
    ("a145" . "&#x0CC;")  		; I`
    ("a146" . "&#x254;&#x300;")		; 開いたO`
    ("a147" . "&#x28A;&#x300;")		; ユプシロン`
    ("a148" . "&#x251;&#x300;")		; 筆記体のA`
    ("a149" . "&#x300;")		; 逆向きのアポストロフィー`
    ("a14a" . "&#x0E0;")  		; a`
    ("a14b" . "&#x0E8;")  		; e`
    ("a14c" . "&#x0EC;")  		; i`
    ("a14d" . "&#x0F2;")		; o`
    ("a14e" . "&#x0F9;")  		; u`
    ("a14f" . "&#x28C;&#x300;")		; 逆さのV`
    ("a150" . "&#x28C;")		; 逆さのV
    ("a151" . "&#x0C1;")		; A'
    ("a152" . "B&#x301;")		; B'
    ("a153" . "C&#x301;")		; C'
    ("a154" . "D&#x301;")		; D'
    ("a155" . "&#x0C9;")		; E'
    ("a156" . "F&#x301;")		; F'
    ("a157" . "G&#x301;")		; G'
    ("a158" . "H&#x301;")		; H'
    ("a159" . "&#x0CD;")		; I'
    ("a15a" . "L&#x301;")		; L'
    ("a15b" . "M&#x301;")		; M'
    ("a15c" . "&#x0D3;")		; O'
    ("a15d" . "P&#x301;")		; P'
    ("a15e" . "Q&#x301;")		; Q'
    ("a15f" . "R&#x301;")		; R'
    ("a160" . "S&#x301;")		; S'
    ("a161" . "T&#x301;")		; T'
    ("a162" . "&#x0DA;")		; U'
    ("a163" . "V&#x301;")		; V'
    ("a164" . "X&#x301;")		; X'
    ("a165" . "Y&#x301;")		; Y'
    ("a166" . "Z&#x301;")		; Z'
    ("a167" . "&#x0E1;")  		; a'
    ("a168" . "&#x0E9;")  		; e'
    ("a169" . "&#x0ED;")  		; i'
    ("a16a" . "&#x0F3;")  		; o'
    ("a16b" . "&#x0FA;")  		; u'
    ("a16c" . "&#x0FD;")		; y'
    ("a16d" . "&#x0C0;")		; A`
    ("a16e" . "&#x0C8;")		; E`
    ("a16f" . "&#x0CC;")		; I`
    ("a170" . "&#x0D2;")		; O`
    ;;("a171" . "&#x259;&#x301;")	; シュワー'  a171,a172 ae'
    ;;("a171a172" . "&#x0E6;&#x301;")	; アッシュ; 小文字AとEの合字'
    ;;("a171a172" . "&#x1FD;")		; アッシュ; 小文字AとEの合字'
    ("a171" . "")			; a171,a172 ae'
    ("a172" . "&#x1FD;")		; アッシュ; 小文字AとEの合字'
    ("a172" . "&#x0E9;")		; e'
    ;;("a173" . "&#x259;&#x300;")	; シュワー`
    ;;("a174" . "&#x0E8;")		; e`
    ;;("a173a174" . "&#x0E6;&#x300;")	; アッシュ; 小文字AとEの合字`
    ("a173" . "")			; a173,a174 ae`
    ("a174" . "&#x0E6;&#x300;")		; アッシュ; 小文字AとEの合字`
    ;;("a175" . "&#x259;")		; シュワー a175,a176 ae
    ;;("a176" . "&#x065;")  		; 小文字のE
    ;;("a175a176" . "&#x0E6;")		; アッシュ; 小文字AとEの合字
    ("a175" . "")			; a175,a176 ae
    ("a176" . "&#x0E6;")		; アッシュ; 小文字AとEの合字
    ("a177" . "S&#x300;")		; S`
    ("a178" . "T&#x300;")		; T`
    ("a179" . "&#x0D9;")		; U`
    ("a17a" . "V&#x300;")		; V`
    ("a17b" . "&#x0E0;")  		; a`
    ("a17c" . "&#x0E8;")  		; e`
    ("a17d" . "&#x0EC;")  		; i`
    ("a17e" . "&#x0F2;")  		; o`
    ;; a17f-a220なし
    ("a221" . "&#x0F9;")  		; u`
    ("a222" . "y&#x300;")		; y`
    ("a223" . "☆")
    ("a224" . "☆")
    ("a225" . "☆")
    ("a226" . "&#x259;")  		; シュワー
    ("a227" . "&#x25A;")  		; 右鉤付きのシュワー
    ("a228" . "&#x26A;")		; 小型大文字のI
    ("a229" . "&#x254;")		; 開いたO
    ("a22a" . "&#x28A;")  		; ユプシロン
    ("a22b" . "&#x3B8;")  		; テータ(シータ)
    ("a22c" . "&#x0F0;")  		; エズ
    ("a22d" . "&#x283;")  		; エッシュ
    ("a22e" . "&#x292;")  		; エッジュ; 尾付きのZ
    ("a22f" . "&#x14B;")  		; エング
    ("a230" . "☆")
    ("a231" . "☆")
    ("a232" . "&#x294;")		; ?
    ("a233" . "&#x2D0;")  		; 長音符
    ("a234" . "&#x251;")		; 筆記体のA
    ("a235" . "☆")
    ("a236" . "☆")
    ("a237" . "&#x0E3;")		; a~
    ("a238" . "&#x0F1;")		; n~
    ("a239" . "☆")
    ("a23a" . "☆")
    ("a23b" . "&#x1AB;")  		; 左向き鉤付きのT
    ("a23c" . "☆")
    ("a23d" . "☆")
    ("a23e" . "&#x0D6;")		; Oウムラウト
    ("a23f" . "&#x0E4;")		; aウムラウト
    ("a240" . "&#x0EB;")		; eウムラウト
    ("a241" . "&#x0EF;")		; iウムラウト
    ("a242" . "&#x0F6;")		; oウムラウト
    ("a243" . "&#x0FC;")		; uウムラウト
    ("a244" . "&#x2C6;")		; ^
    ("a245" . "&#x0E2;")		; a^
    ("a246" . "&#x0EA;")		; e^
    ("a247" . "&#x0EE;")		; i^
    ("a248" . "&#x0EA;")		; o^
    ("a249" . "&#x2C9;")		; -
    ;;("a24a" . "&#x251;&#x304;")		; 筆記体のA-
    ("a24a" . "&#x101;")		; a-
    ("a24b" . "&#x113;")		; e-
    ("a24c" . "&#x12B;")		; i-
    ("a24d" . "&#x14D;")		; o-
    ("a24e" . "&#x16B;")		; u-
    ("a24f" . "☆")
    ("a250" . "☆")
    ("a251" . "☆")
    ("a252" . "☆")
    ("a253" . "☆")
    ("a254" . "☆")
    ("a255" . "☆")
    ("a256" . "☆")
    ("a257" . "☆")
    ("a258" . "☆")
    ("a259" . "☆")
    ("a25a" . "☆")
    ("a25b" . "☆")
    ("a25c" . "☆")
    ("a25d" . "☆")
    ("a25e" . "☆")
    ("a25f" . "☆")
    ("a260" . "☆")
    ("a261" . "☆")
    ("a262" . "☆")
    ("a263" . "☆")
    ("a264" . "☆")
    ("a265" . "☆")
    ("a266" . "☆")
    ("a267" . "☆")
    ("a268" . "☆")
    ("a269" . "☆")
    ("a26a" . "☆")
    ("a26b" . "・")
    ("a26c" . "&#x0D1;")		; N~
    ("a26d" . "E&#x300;")		; E`
    ("a26e" . "C&#x300;")		; C`
    ("a26f" . "D&#x300;")		; D`
    ("a270" . "G&#x300;")		; G`
    ("a271" . "N&#x300;")		; N`
    ("a272" . "P&#x300;")		; P`
    ("a273" . "Q&#x300;")		; Q`
    ("a274" . "☆")
    ("a275" . "☆")
    ("a276" . "☆")
    ("a277" . "☆")
    ("a278" . "☆")
    ("a279" . "☆")
    ("a27a" . "<sup>&#x259;</sup>")	; シュワー
    ;; a27b-a320なし
    ("a321" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>[名]</strong></a>")
    ("a322" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>[代]</strong></a>")
    ("a323" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>[形]</strong></a>")
    ("a324" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>[動]</strong></a>")
    ("a325" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>[副]</strong></a>")
    ("a326" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>[接]</strong></a>")
    ("a327" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>[前]</strong></a>")
    ("a328" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>[冠]</strong></a>")
    ("a329" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>[間]</strong></a>")
    ("a32a" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>[助</strong></a>")
    ("a32b" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>動]</strong></a>")
    ("a32c" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>[接</strong></a>")
    ("a32d" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>頭]</strong></a>")
    ("a32e" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>尾]</strong></a>")
    ("a32f" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078142&offset=1398&frommenu=true\"><strong>[U]</strong></a>")
    ("a330" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078142&offset=1398&frommenu=true\"><strong>[C]</strong></a>")
    ("a331" . "<a href=\"?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1464&frommenu=true\">(単)</a>")
    ("a332" . "<a href=\"?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1464&frommenu=true\">(複)</a>")
    ("a333" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078142&offset=0478&frommenu=true\"><strong>[A]</strong></a>")
    ("a334" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078142&offset=0478&frommenu=true\"><strong>[P]</strong></a>")
    ("a335" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>(自)</strong></a>")
    ("a336" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078138&offset=1038&frommenu=true\"><strong>(他)</strong></a>")
    ("a337" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078148&offset=1848&frommenu=true\"><strong>[成</strong></a>")
    ("a338" . "<a href=\"./?search=&match=&dictionary=NEW_EJJE&block=00078148&offset=1848&frommenu=true\"><strong>句]</strong></a>")
    ("a339" . "♪")
    ("a33a" . "用例")
    ("a33b" . "☆")
    ("a33c" . "品詞一覧")
    ("a33d" . "||")
    ("a33e" . "|| ")
    ("a33f" . "⇒")
    ("a340" . "&#x334;&#x301;")		; ~'
    ("a341" . "&#x334;&#x300;")		; ~`
    ("a342" . "☆")
    ("a343" . "&#x2935;")		; 斜め下向き矢印
    ("a344" . "&#x2934;")		; 斜め上向き矢印
    ("a345" . "☆")
    ("a346" . "☆")
    ("a347" . "☆")
    ("a348" . "☆")
    ("a349" . "☆")
    ("a34a" . "☆")
    ("a34b" . "☆")
    ("a34c" . "☆")
    ("a34d" . "&#x2026;&#x301;")	; …'
    ("a34e" . "—")
    ("a34f" . "⇔")

    ;; computer
    ("b125" . "—")
    ("b128" . "[名]")
    ("b12b" . "[動]")
    )
  "excite英和辞典の発音記号とフォントの変換テーブル。
Fix me!"
  ;; http://yue.sakura.ne.jp/melody/ware/iso88591.html
  ;; http://yue.sakura.ne.jp/melody/ware/all-code-list.html
  ;; http://www.geocities.jp/mura_yosi/js/
  ;; http://www.fiberbit.net/user/hobbit-t/html/uniipad.html
  ;; http://ja.wikipedia.org/wiki/国際音声記号の文字一覧
  ;; http://ja.wikipedia.org/wiki/Unicode#.E4.B8.80.E8.A6.A7
  )

(defvar dic-lookup-w3m-filter-excite-jj-symbol-alist
  '(
    ("GE040" . "&#x3280;")		; 丸一
    ("GE041" . "&#x3281;")		; 丸二
    ("G6971" . "&#x2776;")		; 丸1
    ("G6972" . "&#x2777;")		; 丸2
    ("G6973" . "&#x2778;")		; 丸3
    ("G6974" . "&#x2779;")		; 丸4
    ("G6975" . "&#x277A;")		; 丸5
    ("G6976" . "&#x277B;")		; 丸6
    ("G6977" . "&#x277C;")		; 丸7
    ("G6978" . "&#x277D;")		; 丸8
    ("G6979" . "&#x277E;")		; 丸9
    ("sign_man" . "&#x25C9;")		; ◉ 二重丸
    )
  "excite国語辞典の発音記号とフォントの変換テーブル。")

(defvar dic-lookup-w3m-filter-excite-cj-symbol-alist
  '(
    ("shisei_1" . "&#x304;")		; -
    ("shisei_2" . "&#x301;")		; /
    ("shisei_3" . "&#x30C;")		; v
    ("shisei_4" . "&#x300;")		; \
    ("tanyou_fuka" . "&#x2297;")	; otimes
    ("bunri" . "&#x2666;")		; diams
    ("yakugo" . "&#x25BA;")		; 右向き黒三角
    ("youyaku" . "&#x2666;")		; 黒ひし形
    ("yourei" . "(用例) ")
    )
  "excite中日の発音記号とフォントの変換テーブル。")

(defvar dic-lookup-w3m-filter-yahoo-ej1-symbol-alist
  '(
    ("AC0" . "&#x0C0;")		      ; A`
    ("AC1" . "&#x0C1;")		      ; A'
    ("AC4" . "&#x1CD;")		      ; Av
    ("AC8" . "&#x0C8;")		      ; E`
    ("AC9" . "&#x0C9;")		      ; E'
    ("ACD" . "&#x0CD;")		      ; I'
    ("ACE" . "&#x0CE;")		      ; I^
    ("AD2" . "&#x0D2;")		      ; O`
    ("AD3" . "&#x0D3;")		      ; O'
    ("AD4" . "&#x0D4;")		      ; O^
    ("AD6" . "&#x14E;")		      ; Ov
    ("AD9" . "&#x0D9;")		      ; U`
    ("ADA" . "&#x0DA;")		      ; U'
    ("ADC" . "&#x0DC;")		      ; U..
    ("AE0" . "&#x0E0;")		      ; a`
    ("AE1" . "&#x0E1;")		      ; a'
    ("AE8" . "&#x0E8;")		      ; e`
    ("AE9" . "&#x0E9;")		      ; e'
    ("AEC" . "&#x0EC;")		      ; i`
    ("AED" . "&#x0ED;")		      ; i'
    ("AF0" . "&#x0F0;")		      ; エズ
    ("AF2" . "&#x0F2;")		      ; o`
    ("AF3" . "&#x0F3;")		      ; o'
    ("AF9" . "&#x0F9;")		      ; u`
    ("AFA" . "&#x0FA;")		      ; u'
    ;;("AFD" . "y&#x301;")      ; y'
    ("AFD" . "&#x0FD;")			; y'
    ("C98" . "&#x283;")			; エッシュ
    ("D24" . "&#x1CE;")			; av
    ("D26" . "&#x101;")			; a-
    ("D2A" . "&#x103;")			; au
    ("D2D" . "&#x105;")			; a,
    ("D30" . "&#x251;")			; 筆記体のA
    ("D31" . "&#x251;&#x300;")		; 筆記体のA`
    ("D32" . "&#x251;&#x301;")		; 筆記体のA'
    ("D40" . "&#x0E6;")			; アッシュ; 小文字AとEの合字
    ("D41" . "&#x0E6;&#x300;")		; アッシュ; 小文字AとEの合字`
    ;;("D42" . "&#x0E6;&#x301;")	; アッシュ; 小文字AとEの合字'
    ("D42" . "&#x1FD;")			; アッシュ; 小文字AとEの合字'
    ("D5D" . "&#x254;&#x301;")		; 開いたO'
    ("D5E" . "&#x254;&#x300;")		; 開いたO`
    ("D6D" . "&#x254;")			; 開いたO
    ("D84" . "&#x11B;")			; ev
    ("D86" . "&#x113;")			; e-
    ("D87" . "&#x0E8;")			; e`
    ("D90" . "&#x259;")			; シュワー
    ("D91" . "&#x259;&#x300;")		; シュワー`
    ("D92" . "&#x259;&#x301;")		; シュワー'
    ("E5B" . "&#x14B;")  		; エング
    ("F2A" . "&#x28C;&#x301;")		; 逆さのV'
    ("F2B" . "&#x28C;&#x300;")		; 逆さのV`
    ("F2C" . "&#x28C;")			; 逆さのV
    ("F51" . "y&#x300;")		; y`
    ("FB1" . "&#x25B;&#x300;")		; エプシロン`
    ("FB2" . "&#x25B;&#x301;")		; エプシロン'
    ("FBE" . " &#x2013;&#x301; ")	; -'
    ("FBF" . " &#x2013;&#x300; ")	; -`
    ("FC3" . "&#x292;")  		; エッジュ; 尾付きのZ
    ("G41" . "&#x261;")			; 開いた尾のG
    ("_817C" . "&#x2D0;")		; 長音符
    ("Z8616" . "【1】")
    ("Z6AFA" . "【2】")
    ("Z6B50" . "【1】")
    ("Z6B59" . "【2】")
    ("ar_next" . "&#x25BA;")		; 右向き黒三角
    ("T2460" . "&#x2460;")		; 丸1
    ("T2461" . "&#x2461;")		; 丸2
    ("T2462" . "&#x2462;")		; 丸3
    ("T2463" . "&#x2463;")		; 丸4
    ("T2464" . "&#x2464;")		; 丸5
    ("T2465" . "&#x2465;")		; 丸6
    ("T2466" . "&#x2466;")		; 丸7
    ("Z6B83" . "&#x2467;")		; 丸8
    ("Z6B8D" . "&#x2468;")		; 丸9
    ("Z6B98" . "&#x2469;")		; 丸10
    )
  "yahooプログレッシブ英和中辞典の発音記号とフォントの変換テーブル。
Fix me!"
  ;; http://yue.sakura.ne.jp/melody/ware/iso88591.html
  ;; http://yue.sakura.ne.jp/melody/ware/all-code-list.html
  ;; http://www.geocities.jp/mura_yosi/js/
  ;; http://www.fiberbit.net/user/hobbit-t/html/uniipad.html
  ;; http://ja.wikipedia.org/wiki/国際音声記号の文字一覧
  ;; http://ja.wikipedia.org/wiki/Unicode#.E4.B8.80.E8.A6.A7
  )

(defvar dic-lookup-w3m-filter-yahoo-ej2-symbol-alist
  '(
    ("g10d4" . "&#x2020;")		; ダガー
    ("g111a" . "≪")
    ("g111b" . "≫")
    ("g111c" . "&#x2021;")		; ダブルダガー
    ("g111d" . "*")
    ("g111e" . "(同)")
    ("g1128" . "(移)")
    ("g1129" . "(自)")
    ("g112a" . "(他)")
    ("g112b" . "(単)")
    ("g112d" . "(複)")
    ("g112e" . "[C/]")
    ("g112f" . "[C]")
    ("g1130" . "[U]")
    ("g1131" . "[UC]")
    ("g1132" . "[aU]")
    ("g1133" . "[e]")
    ("g1134" . "[m]")
    ("g1135" . "[過]")
    ("g1136" . "[過分]")
    ("g1137" . "[間]")
    ("g1138" . "[形]")
    ("g1139" . "[助]")
    ("g113b" . "[接]")
    ("g113e" . "[前]")
    ("g113f" . "[代]")
    ("g1142" . "[動]")
    ("g1144" . "[副]")
    ("g1145" . "[名]")
    ("g1147" . "&#x2194;")		; ⇔
    ("g1198" . "&#x2021;&#x2021;")	; ダブルダガー x2
    ("g119a" . "&#x25B9;")		; 右向き白抜き三角
    ("g11b9" . "〔")
    ("g11ba" . "〕")
    ("g11da" . "&#x2013; ")		; -
    ("g11db" . "&#x2013;&#x301; ")	; -'
    ("g11dc" . "&#x2013;&#x300; ")	; -`
    ("g11f1" . "—")
    ("g11f5" . "&#x251;")		; 筆記体のA
    ("g11fd" . "&#x251;&#x301;")	; 筆記体のA'
    ("g11fe" . "&#x251;&#x300;")	; 筆記体のA`
    ("g120f" . "&#x261;")		; 開いた尾のG
    ("g122b" . "&#x27e;")		; 釣針のR
    ("g1294" . "&#x254;")		; 開いたO
    ("g1295" . "&#x28C;")		; 逆さのV
    ("g1296" . "&#x0F0;")  		; エズ
    ("g1298" . "&#x3B8;")  		; テータ(シータ)
    ("g1297" . "&#x14B;")  		; エング
    ("g129a" . "&#x292;")  		; エッジュ; 尾付きのZ
    ("g129b" . "&#x259;")  		; シュワー
    ("g129c" . "&#x283;")  		; エッシュ
    ("g129e" . "&#x1FD;")		; アッシュ; 小文字AとEの合字'
    ("g129f" . "&#x0E6;&#x300;")	; アッシュ; 小文字AとEの合字`
    ("g12a0" . "&#x254;&#x301;")	; 開いたO'
    ("g12a1" . "&#x254;&#x300;")	; 開いたO`
    ("g12a2" . "&#x28C;&#x301;")	; 逆さのV'
    ("g12a3" . "&#x28C;&#x300;")	; 逆さのV`
    ("g12a4" . "&#x259;&#x301;")	; シュワー'
    ("g12a5" . "&#x259;&#x300;")	; シュワー`
    ("g12a6" . "&#x25B;")		; エプシロン
    ("g12a7" . "&#x0E6;")		; アッシュ; 小文字AとEの合字
    ("g12c9" . "&#x2020;")		; ダガー
    ("g12cf" . "&#x2D0;")		; 長音符
    ("agrave" . "&#x0E0;")		; a`
    ("aacute" . "&#x0E1;")		; a'
    ("egrave" . "&#x0E8;")		; e`
    ("eacute" . "&#x0E9;")		; e'
    ("igrave" . "&#x0EC;")		; i`
    ("iacute" . "&#x0ED;")  		; i'
    ("ograve" . "&#x0F2;")  		; o`
    ("oacute" . "&#x0F3;")  		; o'
    ("ugrave" . "&#x0F9;")  		; u`
    ("uacute" . "&#x0FA;")  		; u'
    ("Agrave" . "&#x0C0;")		; A`
    ("Aacute" . "&#x0C1;")		; A'
    ("Egrave" . "&#x0C8;")		; E`
    ("Eacute" . "&#x0C9;")		; E'
    ("Igrave" . "&#x0CC;")		; I`
    ("Iacute" . "&#x0CD;")		; I'
    ("Ograve" . "&#x0D2;")		; O`
    ("Oacute" . "&#x0D3;")		; O'
    ("Ugrave" . "&#x0D9;")		; U`
    ("Uacute" . "&#x0DA;")		; U'
    ("audio" . "♪")
    )
  "yahoo新グローバル英和辞典の発音記号とフォントの変換テーブル。
Fix me!")

(defvar dic-lookup-w3m-filter-ocn-ej-symbol-alist
  '(
    ;;("fukugou" . "⇒")
    ("fukugou" . "&#x25BA;")		; 右向き黒三角
    ;;("seiku" . "→")
    ("seiku" . "&#x25B9;")		; 右向き白抜き三角
    ;;("hasei" . "[派]")
    ("hasei" . "&#x2666;")
    ("mp3" . "[MP3]")
    ("wav" . "[WAV]")
    ("voice" . "≪♪")
    ("ej_btn" . "[英和]")
    ("je_btn" . "[和英]")
    ("jn_btn" . "[国語]")
    ("nw" . "<strong>[新語]</strong>")
    ("i_01s" . "・")
    ("clear" . "—")
    ("e1000" . "&#x0E6;")		; アッシュ; 小文字AとEの合字
    ("e1001" . "b")
    ("e1002" . "s")
    ("e1003" . "t")
    ("e1004" . "&#x259;&#x301;")	; シュワー'
    ("e1005" . "&#x2D0;")		; 長音符
    ("e1006" . "<em>r</em>")
    ("e1007" . "d")
    ("e1008" . "&#x292;")  		; エッジュ; 尾付きのZ
    ("e1009" . "&#x259;")  		; シュワー
    ("e100a" . "n")
    ("e100b" . "/")
    ("e100c" . "-")
    ("e100d" . "a")
    ("e100e" . "c")
    ("e100f" . "e")
    ("e1010" . "f")
    ("e1011" . "j")
    ("e1012" . "h")
    ("e1013" . "i")
    ("e1014" . "g")
    ("e1015" . "k")
    ("e1016" . "l")
    ("e1017" . "m")
    ("e1018" . "o")
    ("e1019" . "p")
    ("e101a" . "q")
    ("e101b" . "r")
    ("e101c" . "u")
    ("e101d" . "v")
    ("e101e" . "w")
    ("e101f" . "x")
    ("e1020" . "y")
    ("e1021" . "z")
    ("e1022" . "&#x1FD;")		; アッシュ; 小文字AとEの合字'
    ("e1023" . "&#x0E6;&#x300;")	; アッシュ; 小文字AとEの合字`
    ("e1024" . "&#x0E6;&#x303;")	; アッシュ; 小文字AとEの合字~
    ("e1025" . "&#x0E6;&#x303;&#x301;") ; アッシュ; 小文字AとEの合字~'
    ;;("e1026" . "&#x25A;")		; 右鉤付きのシュワー
    ;;("e1026" . "&#x259;")		; シュワー
    ("e1026" . "<em>&#x259;</em>")	; シュワー
    ("e1027" . "&#x259;&#x300;")	; シュワー`
    ("e1028" . "&#x251;")		; 筆記体のA
    ("e1029" . "&#x251;&#x301;")	; 筆記体のA'
    ("e102a" . "&#x251;&#x300;")	; 筆記体のA`
    ("e102b" . "&#x251;&#x303;")	; 筆記体のA~
    ("e102c" . "&#x251;&#x303;&#x301;")	; 筆記体のA~'
    ("e102d" . "&#x251;&#x303;&#x300;")	; 筆記体のA~`
    ("e102e" . "&#x25B;")		; エプシロン
    ("e102f" . "&#x25B;&#x301;")	; エプシロン'
    ("e1030" . "&#x25B;&#x300;")	; エプシロン`
    ("e1031" . "&#x25B;&#x303;")	; エプシロン~
    ("e1032" . "&#x25B;&#x303;&#x301;")	; エプシロン~'
    ("e1033" . "&#x25B;&#x303;&#x300;")	; エプシロン`
    ("e1034" . "&#x1E3E;")		; M'
    ;;("e1034" . "M&#x301;")		; M'
    ("e1035" . "&#x0ED;")		; I'
    ("e1036" . "&#x14B;")  		; エング
    ("e1037" . "&#x254;")		; 開いたO
    ("e1038" . "&#x254;&#x301;")	; 開いたO'
    ("e1039" . "&#x254;&#x300;")	; 開いたO`
    ("e103a" . "&#x254;&#x303;")	; 開いたO~
    ("e103b" . "&#x254;&#x303;&#x301;")	; 開いたO~'
    ("e103c" . "&#x254;&#x303;&#x300;")	; 開いたO~`
    ("e103d" . "&#x0F0;")  		; エズ
    ("e103e" . "T&#x301;")		; T'
    ("e103f" . "&#x0DA;")		; U'
    ("e1040" . "V&#x301;")		; V'
    ("e1041" . "&#x0DD;")		; Y'
    ("e1042" . "&#x0294;")		; ?
    ("e1043" . "&#x28C;")		; 逆さのV
    ("e1044" . "&#x28C;&#x301;")	; 逆さのV'
    ("e1045" . "&#x28C;&#x303;")	; 逆さのV`
    ("e1046" . "&#x28C;&#x303;")	; 逆さのV~
    ("e1047" . "&#x0E1;")		; a'
    ("e1048" . "&#x0E0;")  		; a`
    ("e1049" . "&#x0E2;")		; a^
    ("e104a" . "&#x0E3;")		; a~
    ("e104b" . "&#x0E3;&#x301;")	; a~'
    ("e104c" . "&#x0E3;&#x300;")	; a~`
    ("e104d" . "&#x0E5;")		; a。
    ("e104e" . "&#x0E4;")		; a..
    ("e104f" . "&#x101;")		; a-
    ("e1050" . "&#x0E7;")		; セディーユ付きのC
    ;;("e1051" . "&#x297;&#x0B8;")	; 縦長のC セディーユ
    ("e1051" . "<em>&#x0E7;</em>")	; 縦長のC セディーユ
    ("e1052" . "&#x0F0;")  		; エズ
    ("e1053" . "&#x0E9;")		; e'
    ("e1054" . "&#x0E8;")		; e`
    ("e1055" . "&#x0EA;")		; e^
    ("e1056" . "&#x0EB;")		; e..
    ("e1057" . "&#x113;")		; e-
    ("e1058" . "<em>&#x261;</em>")	; g
    ;;e1059なし
    ("e105a" . "&#x0CD;")		; I'
    ("e105b" . "&#x0CC;")		; I`
    ("e105c" . "&#x0CE;")		; I^
    ("e105d" . "&#x0CF;")		; I..
    ("e105e" . "<em>j</em>")
    ("e105f" . "m&#x325;")		; m下丸
    ("e1060" . "m&#x325;&#x301;")	; m下丸'
    ("e1061" . "&#x0F1;")		; n~
    ("e1062" . "&#x0F3;")  		; o'
    ("e1063" . "&#x0F2;")  		; o`
    ("e1064" . "&#x0F4;")		; o^
    ("e1065" . "&#x0F6;")		; o..
    ("e1066" . "r&#x302;")		; r^
    ("e1067" . "&#x3B8;")  		; テータ(シータ)
    ("e1068" . "&#x0FA;")		; u'
    ("e1069" . "&#x0F9;")		; u`
    ("e106a" . "&#x0FB;")		; u^
    ("e106b" . "&#x0FC;")		; u..
    ("e106c" . "&#x0FD;")		; y'
    ("e106d" . "y&#x300;")		; y`
    ("e106e" . "&#x334;&#x301;")	; ~'
    ("e106f" . "&#x334;&#x300;")	; ~`
    ("e1070" . "&#x2013;")		; -
    ("e1071" . "&#x2013;&#x301;")	; -'
    ("e1072" . "&#x2013;&#x300;")	; -`
    ("e1073" . "〔")
    ("e1074" . "〕")
    ("e1075" . "強")
    ("e1076" . "<em>Sp.</em>")
    ("e1077" . "<em>Flem.</em>")
    ("e1078" . "<em>Port.</em>")
    ("e1079" . "<em>It.</em>")
    ("e107a" . "<em>F.</em>")
    ("e107b" . "<em>G.</em>")
    ("e107c" . "&#x265;")		; 逆さのH
    ("e107d" . "&#x272;")		; (左側に)左向き尾付きのN
    ("e107e" . "&#x153;")		; 小文字のOとEの合字
    ("e107f" . "&#x0F8;")		; o/
    ("e1080" . "&#x153;&#x303;")	; 小文字のOとEの合字~
    ("e1081" . "&#x0F8;&#x301;")	; o/'
    ("e1082" . "&#x1D2;")		; ov
    ("e1083" . "&#x1D0;")		; iv
    ("e1084" . "&#x1CE;")		; av
    ("e1085" . "n&#x304;")		; n-
    ("e1086" . "&#x0C5;")		; A。
    ;;("e1087" . "&#x276;")		; 小型大文字OとEの合字
    ("e1087" . "&#x152;")		; 小型大文字OとEの合字
    ("e1088" . "&#x163;")		; t,
    ("e1089" . "<em>b</em>")
    ("e108a" . "<em>d</em>")
    ("e108b" . "<em>f</em>")
    ("e108c" . "<em>h</em>")
    ("e108d" . "<em>i</em>")
    ("e108e" . "<em>k</em>")
    ("e108f" . "<em>p</em>")
    ("e1090" . "<em>t</em>")
    ("e1091" . "<em>u</em>")
    )
  "OCN EXCEED英和辞典の発音記号とフォントの変換テーブル。
Fix me!")

(defvar dic-lookup-w3m-filter-infoseek-ej-symbol-alist
  '(
    ("GRA0001" . "&#x0E6;")		; アッシュ; 小文字AとEの合字
    ("GRA0002" . "&#x1FD;")		; アッシュ; 小文字AとEの合字'
    ("GRA0003" . "&#x0E6;&#x300;")	; アッシュ; 小文字AとEの合字`
    ("GRA0004" . "&#x259;")  		; シュワー
    ("GRA0005" . "&#x259;&#x301;")	; シュワー'
    ("GRA0006" . "&#x259;&#x300;")	; シュワー`
    ("GRA0007" . "&#x153;")		; 小文字のOとEの合字
    ("GRA0008" . "&#x153;&#x301;")	; 小文字のOとEの合字'
    ("GRA0009" . "&#x153;&#x300;")	; 小文字のOとEの合字`
    ("GRA0010" . "&#x153;&#x303;")	; 小文字のOとEの合字~
    ("GRA0011" . "&#x3B2;")		; ベータ
    ("GRA0012" . "&#x251;")		; 筆記体のA
    ("GRA0013" . "&#x251;&#x301;")	; 筆記体のA'
    ("GRA0014" . "&#x251;&#x300;")	; 筆記体のA`
    ("GRA0015" . "&#x251;&#x303;")	; 筆記体のA~
    ("GRA0016" . "&#x25B;")		; エプシロン
    ("GRA0017" . "&#x25B;&#x303;")	; エプシロン~
    ("GRA0018" . "&#x265;")		; 逆さのH
    ("GRA0019" . "&#x14B;")  		; エング
    ("GRA0021" . "&#x254;")		; 開いたO
    ("GRA0022" . "&#x254;&#x301;")	; 開いたO'
    ("GRA0023" . "&#x254;&#x300;")	; 開いたO`
    ("GRA0024" . "&#x254;&#x303;")	; 開いたO~
    ("GRA0025" . "&#x283;")  		; エッシュ
    ("GRA0026" . "&#x28C;")		; 逆さのV
    ("GRA0027" . "&#x28C;&#x301;")	; 逆さのV'
    ("GRA0028" . "&#x28C;&#x300;")	; 逆さのV`
    ("GRA0029" . "&#x0E1;")		; a'
    ("GRA0030" . "&#x0E0;")		; a`
    ("GRA0031" . "&#x0E3;")		; a~
    ("GRA0032" . "&#x0E7;")		; セディーユ付きのC
    ("GRA0033" . "&#x0F0;")  		; エズ
    ("GRA0034" . "&#x0E9;")		; e'
    ("GRA0035" . "&#x0E8;")		; e`
    ("GRA0036" . "e&#x303;")		; e~
    ("GRA0037" . "&#x261;")		; 開いた尾のG
    ("GRA0038" . "&#x292;")  		; エッジュ; 尾付きのZ
    ("GRA0039" . "&#x0ED;")  		; i'
    ("GRA0040" . "&#x0EC;")  		; i`
    ("GRA0041" . "&#x142;")		; l/
    ("GRA0042" . "&#x0F3;")  		; o'
    ("GRA0043" . "&#x0F2;")		; o`
    ("GRA0044" . "&#x0F5;")		; o~
    ("GRA0045" . "&#x0F8;")		; 斜線付きのO
    ("GRA0046" . "&#x01FF;")		; 斜線付きのO'
    ("GRA0047" . "&#x3B8;")  		; テータ(シータ)
    ("GRA0048" . "&#x0FA;")		; u'
    ("GRA0049" . "&#x0F9;")		; u`
    ("GRA0050" . "&#x0FD;")		; y'
    ("GRA0051" . "&#x2013; ")		; -
    ("GRA0052" . "&#x2013;&#x301; ")	; -'
    ("GRA0053" . "&#x2013;&#x300; ")	; -`
    ("GRA0054" . "&#x0294;")		; ?
    ("ic_eiwa" . "<strong>[E]</strong>")
    ("ic_waei" . "<strong>[J]</strong>")
    ("ic_kokugo" . "<strong>[大]</strong>")
    ("icon02" ."[*]")
    ("icon_honyaku" . "[訳]")
    ("icon_kanji" . "[漢]")
    ("icon_all" . "[全]")
    ("icon_eiwa" . "[EJ]")
    ("icon_waei" . "[JE]")
    ("icon_kokugo" . "[大]")
    ("icon_kana" . "[カ]")
    )
  "infoseek 英和辞典の発音記号とフォントの変換テーブル。
Fix me!")

;; (defvar dic-lookup-w3m-filter-weblio-ej-symbol-alist
;;   `(
;;     ,@(mapcar
;;        #'(lambda (elem)
;; 	  (cons (concat "N16-" (upcase (car elem)) "_F-000000_B-FFFFFF")
;; 		(cdr elem)))
;;        dic-lookup-w3m-filter-excite-ej-symbol-alist)
;;
;;     ("W16-A343_F-000000_B-FFFFFF" . "&#x2198;") ; 斜め下向き矢印
;;     ("W16-A344_F-000000_B-FFFFFF" . "&#x2197;") ; 斜め上向き矢印
;;     ("W16-A34D_F-000000_B-FFFFFF" . " &#x2026;&#x301; ") ; …'
;;     ("W16-A328_F-000000_B-FFFFFF" . "[冠]")
;;
;;     ("iconEjjeWav" . "♪")
;;     ("lg_liscj" . "")
;;     ("lg_kejje" . "")
;;     ("lg_kejcy" . "")
;;     ("hand" . "&#x261E;")
;;     ("link_out" . "⇔")
;;     ("icon_bulb" . "・")
;;     ("bulb5" . "[*****]")
;;     ("bulb4" . "[****&nbsp;]")
;;     ("bulb3" . "[***&nbsp;&nbsp;]")
;;     ("bulb2" . "[**&nbsp;&nbsp;&nbsp;]")
;;     ("bulb1" . "[*&nbsp;&nbsp;&nbsp;&nbsp;]")
;;     ("bulb0" . "[&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;]")
;;     ("IconCircleGr" . "◎")
;;     ("IconArrGry" . "・")
;;     ("iconArrGryR" . "≫")
;;     ("spacer" . "")
;;     ("subCategoryPlus" . "")
;;     ("iconCclBl" . "- ")
;;     )
;;   "weblio 英和辞典の発音記号とフォントの変換テーブル。
;; Fix me!")

(defvar dic-lookup-w3m-filter-onlinedic-symbol-alist
  '(("title_1" . "")
    ("title_l1" . "")
    ("title_r1" . "")
    ("bg_1" . "")
    ("end_1" . "<hr>")
    ("arrow3" . "→")
    ("spacer" . " ")
    ("lucky" . "")
    )
  "敦煌辞海の記号とフォントの変換テーブル。")

(defvar dic-lookup-w3m-filter-hjenglish-symbol-alist
  '(("icon_star" . "* ")
    ("spacer" . "")
    ("btn_myword_add" . "(+)")
    ("btn_newword" . "<strong>[new]</strong>")
    ("btn_noresult" . "<strong>[??]</strong>")
    )
  "hjenglishの記号とフォントの変換テーブル。")

(defvar dic-lookup-w3m-filter-kitajiro-pinyin-alist
  '(("ang1" . "&#257;ng") ("ang2" . "&#225;ng")
    ("ang3" . "&#462;ng") ("ang4" . "&#224;ng")
    ("eng1" . "&#275;ng") ("eng2" . "&#233;ng")
    ("eng3" . "&#283;ng") ("eng4" . "&#232;ng")
    ("ing1" . "&#299;ng") ("ing2" . "&#237;ng")
    ("ing3" . "&#464;ng") ("ing4" . "&#236;ng")
    ("ong1" . "&#333;ng") ("ong2" . "&#243;ng")
    ("ong3" . "&#466;ng") ("ong4" . "&#242;ng")
    ("ai1" . "&#257;i") ("ai2" . "&#225;i")
    ("ai3" . "&#462;i") ("ai4" . "&#224;i")
    ("an1" . "&#257;n") ("an2" . "&#225;n")
    ("an3" . "&#462;n") ("an4" . "&#224;n")
    ("ao1" . "&#257;o") ("ao2" . "&#225;o")
    ("ao3" . "&#462;o") ("ao4" . "&#224;o")
    ("ei1" . "&#275;i") ("ei2" . "&#233;i")
    ("ei3" . "&#283;i") ("ei4" . "&#232;i")
    ("en1" . "&#275;n") ("en2" . "&#233;n")
    ("en3" . "&#283;n") ("en4" . "&#232;n")
    ("er1" . "&#275;r") ("er2" . "&#233;r")
    ("er3" . "&#283;r") ("er4" . "&#232;r")
    ("ie1" . "i&#275;") ("ie2" . "i&#233;")
    ("ie3" . "i&#283;") ("ie4" . "i&#232;")
    ("in1" . "&#299;n") ("in2" . "&#237;n")
    ("in3" . "&#464;n") ("in4" . "&#236;n")
    ("ng2" . "&#324;g") ("ng3" . "&#328;g") ("ng4" . "&#505;g")
    ("ou1" . "&#333;u") ("ou2" . "&#243;u")
    ("ou3" . "&#466;u") ("ou4" . "&#242;u")
    ("un1" . "&#363;n") ("un2" . "&#250;n")
    ("un3" . "&#468;n") ("un4" . "&#249;n")
    ("ve3" . "&#252;&#283;") ("ve4" . "&#252;&#232;")
    ("a1" . "&#257;") ("a2" . "&#225;") ("a3" . "&#462;") ("a4" . "&#224;")
    ("e1" . "&#275;") ("e2" . "&#233;") ("e3" . "&#283;") ("e4" . "&#232;")
    ("i1" . "&#299;") ("i2" . "&#237;") ("i3" . "&#464;") ("i4" . "&#236;")
    ("o1" . "&#333;") ("o2" . "&#243;") ("o3" . "&#466;") ("o4" . "&#242;")
    ("u1" . "&#363;") ("u2" . "&#250;") ("u3" . "&#468;") ("u4" . "&#249;")
    ("v1" . "&#470;") ("v2" . "&#472;") ("v3" . "&#474;") ("v4" . "&#476;"))
  "pinyin変換テーブル")

(defun dic-lookup-w3m-filter-conv-pinyin (url regexp)
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (save-excursion
      (save-restriction
	(narrow-to-region (match-beginning 0) (match-end 0))
	(goto-char (point-min))
	(while
	    (re-search-forward
	     "\\(ang\\|eng\\|ing\\|ong\\|ai\\|an\\|ao\\|ei\\|en\\|er\\|ie\\|in\\|ng\\|ou\\|un\\|ve\\|a\\|e\\|i\\|u\\|o\\|v\\)[1-4]"
	     nil t)
	  (replace-match
	   (assoc-default (match-string 0)
			  dic-lookup-w3m-filter-kitajiro-pinyin-alist)
	   t nil))))))

(defvar dic-lookup-w3m-translator-site-list
  '((ej
     (;;("tr-ej-url-ocn" . "ocn")
      ("tr-enja-url-excite-get" . "excite")
      ("tr-ej-url-livedoor" . "livedoor")
      ("tr-ej-url-nifty" . "nifty")
      ("tr-ej-url-sonet" . "sonet")
      ("tr-ej-url-yakushite.net" . "yakushite.net")
      ("tr-enja-url-google" . "google")
      ("tr-enja-url-yahoo" . "yahoo")))
    (jx
     (("tr-jaen-url-excite-get" . "英excite")
      ("tr-je-url-nifty" . "英nifty")
      ;;("tr-je-url-ocn" . "英ocn")
      ("tr-je-url-livedoor" . "英livedoor")
      ("tr-je-url-sonet" . "英sonet")
      ("tr-je-url-yakushite.net" . "英-yakushite.net")
      ("tr-jaen-url-google" . "英google")
      ("tr-jaen-url-yahoo" . "英yahoo")
      ;;("tr-jc-url-ocn" . "中ocn")
      ("tr-jach-url-excite-get" . "中excite")
      ("tr-jc-url-nifty" . "中nifty")
      ("tr-jako-url-excite-get" . "韓excite")
      ;;("tr-jk-url-ocn" . "韓ocn")
      ))
    (cj
     (("tr-chja-url-excite-get" . "excite")
      ("tr-cj-url-nifty" . "nifty")
      ;;("tr-cj-url-ocn" . "ocn")
      ("tr-cj-url-sonet" . "sonet")
      ("tr-cj-url-yakushite.net" . "yakushite.net")
      ("tr-zh-CNja-url-google" . "google")))
    (kj
     (("tr-koja-url-excite-get" . "excite")
      ("tr-kj-url-nifty" . "nifty")
      ;;("tr-kj-url-ocn" . "ocn")
      ("tr-kj-url-sonet" . "sonet")
      ("tr-koja-url-google" . "google"))))
    "*webページを翻訳するtranslatorのリスト。
webページに翻訳ボタンをつけて、各translatorにリンクする。
`dic-lookup-w3m-filter-translation-anchor'で使用。")

(defun dic-lookup-w3m-filter-translation-anchor (url &optional regexp before)
  "webページに翻訳ボタンをつける。"
  (goto-char (point-min))
  ;; いい加減な言語の判定。 Fix me!
  (cond
   ((save-excursion
      (re-search-forward
       "<html [^>]*lang=\"ja[-\"]\\|<meta [^>]*http-equiv=\"content-language\"[^>]*content=\"ja[-\"]" nil t))
    (dic-lookup-w3m-filter-translation-anchor2
     url 'jx "日*翻訳: " regexp before))
   ((or
     (not
      (save-excursion
	(re-search-forward "[^\000-\177]" nil t)))
     (save-excursion
       (re-search-forward
	"<html [^>]*lang=\"en[-\"]\\|<meta [^>]*http-equiv=\"content-language\"[^>]*content=\"en[-\"]\\|<meta [^>]*http-equiv=\"content-type\"[^>]*content=\"text/html; +charset=\\(iso-8859-1\\|us-ascii\\)\"" nil t)))
    (dic-lookup-w3m-filter-translation-anchor2
     url 'ej "英日翻訳: " regexp before))
   ((save-excursion
      (re-search-forward "[ᄀ-ᇹㄱ-ㆎ가-힣]\\{10,\\}" nil t))
    (dic-lookup-w3m-filter-translation-anchor2
     url 'kj "韓日翻訳: " regexp before))
   ((or
     (save-excursion
       (re-search-forward
	"<html [^>]*lang=\"zh-cn\"\\|<meta [^>]*http-equiv=\"content-language\"[^>]*content=\"zh-cn\"\\|<meta [^>]*http-equiv=\"content-type\"[^>]*content=\"text/html; +charset=gb2312\"" nil t))
     (save-excursion
       (re-search-forward "[啊-齄]\\{10,\\}" nil t)))
    (dic-lookup-w3m-filter-translation-anchor2
     url 'cj "中日翻訳: " regexp before))
   ((save-excursion
      (re-search-forward "[あ-ん]" nil t))
    (dic-lookup-w3m-filter-translation-anchor2
     url 'jx "日*翻訳: " regexp before))
   ))

(defun dic-lookup-w3m-filter-translation-anchor2
  (url category heading &optional regexp before)
  (w3m-filter-replace-regexp
   url
   (concat "\\(" (or regexp "<body[^>]*>") "\\)")
   (concat
    (unless before "\\1")
    "<div id=\"dic-lookup-w3m-translation-anchor\">"
    heading
    (mapconcat
     (lambda (s)
       (if (assoc (car s) w3m-search-engine-alist)
	   (format
	    "<a href=\"%s\">%s</a>"
	    (w3m-encode-specials-string
	     (format (nth 1 (assoc (car s) w3m-search-engine-alist))
		     (w3m-url-encode-string
		      url
		      (nth 2 (assoc (car s) w3m-search-engine-alist)))))
	    (w3m-encode-specials-string (cdr s)))
	 (concat (car s) "??")))
     (cadr (assoc category dic-lookup-w3m-translator-site-list))
     ", ")
    "</div><!-- /dic-lookup-w3m-translation-anchor -->"
    (if before "\\1")
    )))

;; http://dic.yahoo.co.jp/ プログレッシブ英和中辞典 |  新グローバル英和辞典
;; http://www.sanseido.net/ デイリーコンサイスシリーズ
;; http://www.excite.co.jp/dictionary/ 新英和中辞典 第６版 （研究社）
;; http://dictionary.goo.ne.jp/  EXCEED 英和辞典
;; http://ocndictionary.goo.ne.jp/ EXCEED 英和辞典
;; http://dictionary.infoseek.ne.jp/
;; http://www.alc.co.jp/ 英辞朗
;; http://dic.livedoor.com/ EXCEED英和辞典
;; http://www.merriam-webster.com/dictionary/ webster
;; http://sara.natcorp.ox.ac.uk/cgi-bin/saraWeb corpus
;; http://www.collins.co.uk/Corpus/CorpusSearch.aspx corpus
;; http://erek.ta2o.net/news/%s.html" corpus
;; http://www.kotonoha.gr.jp/demo/ 日本語コーパス
;; http://www.ctrans.org/cjdic/index.php 中日 (日中)
;; http://www.gengokk.co.jp/thesaurus/ 日本語シソーラス
;; http://ruigo.jp/ 日本語シソーラス
;; http://www.dictjuggler.net/tamatebako/ 日本語シソーラス
;; http://thesaurus.weblio.jp/ 日本語シソーラス
;; http://thesaurus.reference.com/ english thesaurus
;; http://dmoz.atpedia.jp/Reference/Thesauri links to thesaurus sites
;; http://lsd.pharm.kyoto-u.ac.jp/ja/service/weblsd/ ライフサイエンス辞書
;; http://www.tfd.com/
;; http://www.onlinedic.com/search.php
;; http://www.bitex-cn.com/
;; http://www.yakushite.net/
;; http://www.sanseido.net/User/Dic/Index.aspx
;; http://www.stars21.asia/dictionary/Japanese-Chinese_dictionary.html
;; http://www.frelax.com/sc/service/pinyin/ 漢字pinyin変換
;; http://rnnnews.jp/ 時事英語辞典
;; http://www.tranexp.com:2000/Translate/result.shtml
;; http://www.kotoba.ne.jp/ 翻訳のためのインターネットリソース
;; http://www.hir-net.com/link/dic/ オンライン辞書・辞典リンク集
;; http://www.linksyu.com/p32.htm 辞書・文例集
;; http://so-net.web.transer.com/
;; http://homepage2.nifty.com/m_kamada/l_translation.htm
;; http://www.diigo.com/tag/翻訳
;; http://7go.biz/translation/
;; http://lhsp.s206.xrea.com/misc/translation.html
;; http://www.langtolang.com/
;; http://www.kooss.com/honyaku/
;; http://language.tiu.ac.jp/ 
;; http://lucene.jugem.jp/?eid=305
;; http://www.takke.jp/pss/additional_questions.php
;; http://www.linkage-club.co.jp/ExamInfo&Data/BNC lemma Web.txt
;; http://jigen.net/ 字源 部品から漢字を調べる

(provide 'dic-lookup-w3m-ja)

;;; dic-lookup-w3m-ja.el ends here.
