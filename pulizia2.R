pulizia2<-c("c_veri","c_isnt","c_con","c_i","c_netflix","c_ive","c_almost","c_have","c_the","c_one","c_but","c_also","c_how",
            "c_theless","c_wouldnt","c_even","c_can","c_either","c_xor","p_pros","c_thing","c_doesnt", "c_thing","p_netflix", "p_mine",
            "c_havent", "c_dont","c_there", "p_there","c_im","p_im","c_theyr","p_theyr","c_wont","p_wont","c_youv","p_youv",
            "p_made","p_found","c_found","p_thing","p_work","p_ive","p_almost","p_littl","p_have","p_the","p_one","p_but","p_also","p_how",
            "p_theless","p_wouldnt","p_can","p_either","p_xor","p_lot","c_pros","p_doesnt","c_mine","p_havent","p_dont","p_tje","c_arent","p_arent",
            "c_didnt","p_didnt","c_eventu","p_eventu","c_theyv","p_theyv","c_wasnt","p_wasnt","c_whove","p_whove","c_whatnot","p_whatnot","c_hasnt","p_hasnt","p_youll","c_youll","c_youfll","p_youfll",
            "c_holeðÿ","c_arenft","p_arenft","c_canft","p_canft","c_doesnft","p_doesnft","c_donft","p_donft","c_fgreat","p_fgreat","c_aw","p_aw","c_tho","p_tho",
            "p_iffi","p_hes","c_hes","c_altogeth","p_altogeth",
            "c_till","p_till","c_werent","p_werent","c_alram","p_www","p_net","c_aint",
            "p_aint","c_hey","p_hey","c_whatsoev","p_whatsoev","c_shes","p_shes","c_els","p_els","p_isnt","p_theyd","c_theyd","p_jo","c_jo",
            "p_youd","c_youd","p_wouldv","c_wouldv","c_theyll","p_theyll","c_withi","p_withi",
            "p_dang","c_dang","c_wouldv","p_wouldv","c_ten","p_ten","c_allllll","c_hmm","p_hmm","c_ayn","p_ayn","c_hmmm","p_hmmm"
            ,"c_fyi","p_fyi","c_shed","p_shed","p_forti","c_forti","p_altho","c_altho","c_btw","p_btw","c_nflx","p_nflx","c_yess","p_yess","c_whos","p_whos",
            "c_ell","p_ell","c_bammm","p_bammm","c_thirti","p_thirti","c_anymor","p_anymor","c_someth","p_someth","p_lofti","c_lofti","p_con",
            "p_mster","p_ur","c_ur","c_yeah","p_yeah","c_ax","p_ax","p_becasu","c_becasu",
            "p_netlfix","c_piti","p_piti","p_dot","c_dot","c_ah","p_ah","c_yo","p_yo",
            "c_eggager","p_eggager","p_ya","c_accustom","p_accustom","p_devoid","c_devoid",
            "p_outta","c_noy","p_noy","p_chao","c_chao","p_weve","c_weve","c_bye","p_bye","p_doest","c_doest","p_okish","p_thousand","c_thousand","c_xx","p_xx","c_bacaus","p_oneself","c_huh","p_huh","c_realy","p_realy")
corp <- tm_map(corp, removeWords, pulizia2)
corp<-tm_map(corp, stripWhitespace)
tdm<-TermDocumentMatrix(corp)

View(tdm$dimnames$Terms)