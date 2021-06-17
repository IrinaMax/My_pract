# How we can highlight some words in the test using library highlightr
Sometime we need highlight some word in the text. We can do it on R studio using library(highlightr) to achive the result.
 
     #devtools::install_github("JBGruber/highlightr")
     library(highlightr)
     library(tidytext)
     library(syuzhet)
     library(stringr)
     library(tidytext)
Let's take sentence from Wikipedia about data science and try to highlite some words.

     text <-  "Data science is an interdisciplinary field that uses scientific methods, processes, 
              algorithms and systems to extract knowledge and insights from structured and unstructured data,
              and apply knowledge and actionable insights from data."
# 
My idea is create array of some specific words and assign every word to the particular color we wanted to highlight in this sentence
    
    df <- data.frame(
          feature = c("data", "scientific methods", "structured and unstructured","actionable insights", " processes"),
          bg_colour = c("pink", "yellow", "lightblue","lightgreen", "gray"),
          stringsAsFactors = FALSE
     ) 
 Convert df to dictionary with defined color palette
 
     dict <- as_dict(df)
     > dict
        # A tibble: 5 x 6
          feature                       bg_colour  txt_colour bold  italic strike_out
         <chr>                         <chr>      <chr>      <lgl> <lgl>  <lgl>     
        1 "data"                        pink       ""         FALSE FALSE  FALSE     
        2 "scientific methods"          yellow     ""         FALSE FALSE  FALSE     
        3 "structured and unstructured" lightblue  ""         FALSE FALSE  FALSE     
        4 "actionable insights"         lightgreen ""         FALSE FALSE  FALSE     
        5 " processes"                  gray       ""         FALSE FALSE  FALSE     

# Combine the text and dictionary with highlighting backgraound words
      highlight(text, dict)
![Screen Shot 2021-05-30 at 11 10 29 PM](https://user-images.githubusercontent.com/16123495/120147971-5f212a00-c19c-11eb-83eb-2a05085bb804.png)

 Let's try to highlight some words based on the Sentiment score. I will use for this example Lexicon "afinn". My idea is the same: find common words in lexicon form the text , created dictionary and highlight them base on the palette in this case I just appointed "default". But we can priretize some words by stronger color.
 
      text <-  "Good old-fashioned buy-and-hold investing might not be exciting enough to interest day traders. But it can nevertheless produce exciting longer-term       returns and beat out newfangled strategies."
      tokens <-  data.frame(text) %>% unnest_tokens(word, text)
      tokens
               word
         1      good
         2       old
         3 fashioned
         4       buy
         5       and
         6      hold
      # I will pull Lexicon afinn   
      af<- get_sentiments('afinn')
      af %>% summary
            word               value        
     Length:2477        Min.   :-5.0000  
     Class :character   1st Qu.:-2.0000  
     Mode  :character   Median :-2.0000  
                    Mean   :-0.5894  
                    3rd Qu.: 2.0000  
                    Max.   : 5.0000  

     #bn<- get_sentiments('bing')   # can be the other lexicon used as well
     #bn %>% summary
     
     # Pull all common words for the text and Lexicon
     com_words <-tokens %>% inner_join(af)
     com_words
     >Joining, by = "word"
     
     df <- data.frame(
        feature = com_words$word,
        bg_colour = palette("default"),
        stringsAsFactors = FALSE
      )
     df
       feature bg_colour
				1     good     black
    2 exciting   #DF536B
    3 interest   #61D04F
    4 exciting   #2297E6
    5     good   #28E2E5
    6 exciting   #CD0BBC
    7 interest   #F5C710
    8 exciting    gray62
    
    # retrieve words from dictionary and text with coloring background by palette color base on the sentiment score as priority from -5 to 5

      dict1 <- as_dict(df)
      highlight(text,dict1)

![Screen Shot 2021-05-30 at 11 28 19 PM](https://user-images.githubusercontent.com/16123495/120149579-c809a180-c19e-11eb-984f-6730541b96de.png)


    #------------------------------------------------end--------------------
