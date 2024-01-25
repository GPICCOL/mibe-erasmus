# 1. ISCED must be clearly mapped to corsi di studio
# 2. Language should alwasy be the same language
# 3. Livello studio needs to be standardized
# 4. Sedi erasmus must have a codice erasmus as in the DD assignmetns, or the DD assignments must use nome-accordo
# 5. In the DD file the label "no admitted" should be changed






# Per I DOUBLE DEGREE estrarre e valutare SOLO la location dove gli e’ stato assegnato il doble degree (file dd-candidati)
# - Add 50 point to the DD
# - Add 25 points to non-DD
# 
# File SELEZIONI
# - Tabella di conversione da CORSO DI STUDI (selezioni) a ISCED (numerico di 4 cifre)
# - TIPO CORSO STUDI uguaglianza L2 = livello I,  LM = livello II
# 
# 
# FILE SEDI
# - ISCED file SEDI = estrarre 4 cifre da ISCED
# 
# 
# NOTE 
# Controllo dell ISCED non si fa se uno è al terzo anno non si fa il controllo dell ISCED

# 
# DD che hanno una sede che non appare nelle sedi Erasmus, vengono eliminiati dalla lista Erasmus se hanno eventualmente fatto richiesta. 
# (messaggio - lo studente e' stato assegnato a sede DD non Erasumus)
# 
# - Vedere se i dd hanno fatto domanda Erasmus. 
# -- Se NO - No considerarli ulteriormente
# —— se SI - Cancellare tutte le desitnaion diverse da quella che gli e’ stata assegnata in DD (messaggio - Non passa il requisito di conguelnca al DD). 
# 
# 
# BONUS
# - Se lo studente viene dalla lista DD allora "punteggio normalizzato a 100” + 50
# - Tutti gli altri 25. 
# 
# Discrezionale 
# - nel file di output preliminare da mandare alla commission per verifica. Aggiungere colonna “discrezionale” 
# 
# 
# Per finalizzare le allocations, si rifa il matching usando +discrezionale 