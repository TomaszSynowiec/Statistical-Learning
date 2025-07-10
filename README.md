# Predykcja cen domów – Statistical Learning w praktyce

Celem projektu jest analiza regresyjna predykcji **cen domów** na podstawie cech nieruchomości takich jak: lokalizacja, powierzchnia, stan techniczny czy liczba sypialni. Dane pochodzą z lat 2014–2015 i dotyczą sprzedaży domów w **hrabstwie King (Waszyngton, USA)**.

Projekt został zrealizowany w środowisku **RStudio**, a szczególny nacisk położono na:
- eksplorację danych,
- ocenę wpływu zmiennych na wynik,
- porównanie skuteczności wielu modeli regresyjnych,
- analizę błędów RMSE za pomocą **walidacji krzyżowej**.

---

## Opis projektu

Dane zawierają **13 603 obserwacje** oraz 14 zmiennych, takich jak:
- liczba sypialni, metraż, stan techniczny, strefa cenowa,
- czy dom ma piwnicę, był remontowany, ma ładny widok itp.

Dane zostały przygotowane poprzez:
- usunięcie kolumn mało istotnych (data, miesiąc),
- przekształcenie zmiennych logicznych (TRUE/FALSE) na binarne (0/1),
- konwersję zmiennych jakościowych (np. `grade`, `quartile_zone`).

---

## Metody i modele

W projekcie porównano wiele technik modelowania:

- Regresja liniowa (modele pełne i uproszczone)
- Regresja Ridge i Lasso
- Redukcja wymiarów (PCR, PLS)
- Drzewa regresyjne, Bagging, Random Forest
- Boosting, BART, XGBoost

Najlepszy wynik uzyskano za pomocą modelu **BART**:
- **RMSE ≈ 99 896**, co stanowi ok. 25–33% średniej ceny domu.

---

## 💻 Kod

Kod projektu został przygotowany w języku **R** jako notebook lub skrypt.

[Zobacz kod](kod Statistical Learning.R)

---

## 📄 Raport

Pełny raport (PDF) zawiera:
- opis teoretyczny,
- wykresy i interpretacje,
- porównanie wyników modeli.

[Pobierz raport](Raport.pdf)

---
