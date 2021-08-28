# Карчевський М.В. Протидія злочинності в Україні (2013-2020): інфографіка
## **on-line додатки**
Доступність інформаційних технологій та даних офіційної кримінальної статистики дають нагоду використовувати методологію Data Science, здійснюючи цілком відтворювані кримінологічні дослідження. Саме таке дослідження пропонуємо ми. Подані в праці візуалізації дають змогу охарактеризувати зміст даних щодо протидії злочинності в Україні, що містяться у звітах Офісу Генерального прокурора України та Державної судової адміністрації за 2013–2020 роки. Результати роботи може бути використано для формулювання висновків щодо загальних тенденцій протидії злочинності в Україні, а також як емпірична база й підґрунтя побудови гіпотез дальших досліджень. Усі дані оброблялися лише автоматизовано. На їх підставі, знову ж таки програмним способом, будувалися візуалізації, що використовуються для формулювання висновків.  В онлайн-додатках до цієї розвідки представлено вихідні дані, дані, придатні до автоматизованої обробки, програмні коди  для отримання первинних даних, їх обробки й візуалізації, графічні файли візуалізацій.
## Зміст репозитарію
### Скрипти в R
+ **first.R** - забезпечує послідовне виконання всіх скритів проєкту  
+ **get-proceed-COURT.R** - збір та "очищення" даних Державної судової адміністрації України  
+ **get-proceed-PG0.R** - збір та "очищення" даних Офісу Генерального Прокурора України  
+ **load2.R** - завантаження робочої бази даних  
+ **plot1-2.R, plot3.R, plot4-6.R, plot7-9.R, plot10-17.R, plot18-20.R, plot21-23.R, plot_chapter.R, plot_top.R** - скрипти побудови візуалізацій
### Доідники
+ **vocab_court.pdf** - довідник змісту «очищених» даних, отриманих з звітів Державної судової адміністрації України
+ **vocab_pgo.pdf** - довідник змісту «очищених» даних, отриманих з звітів Офісу Генерального Прокурора України
### Первинні дані
+ **RAW_COURT** - звіти ДСА України за 2013-2020 роки (форма 6)
+ **RAW_PGO** - звіти Офісу Генерального Прокурора про обліковані провадження за 2013-2020 роки 
### "Очищені" дані
+ **DATA_COURT** - придатні до автоматизованої обробки дані ДСА (CSV)
+ **DATA_PGO** - придатні до автоматизованої обробки дані Офісу Генерального Прокурора (CSV)
### Візуалізації
+ **GRAPH** - побудовані в результаті виконання скриптів проєкту візуалізації (PNG)
## Як відтворити дослідження
1. Завантажити R (весія не нижче 4.0.5)
2. Завантажити RStudio
3. Розмістити вміст репозитарію до локальної, на Вашому комп'ютері :), теки нового проєкту. Інший спосіб - використовуйте Git Version Control.
4. Завантажити (перевірити наявність) наступних бібліотек R: ***beepr, xlsx, stringr, readxl, tidyverse, scales***
5. Видалити вміст тек з первинними, "очищеними" даними та візуалізаціями у локальній теці нового проєкту
6. Виконати скрипт **first.R**
7. Після закінчення виконання теки вхідних та "очищених" даних, а також візуалізації будь містити відповідні дані

### Бажаю успіхів та приємної роботи!  
### Микола Карчевський  
### **comcriminal@gmail.com**
### **https://karchevskiy.org/**
#### (c) Карчевський М.В., 2021
