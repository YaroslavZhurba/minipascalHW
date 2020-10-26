Интерпретатор для языка Pascal на Haskell

Возможности Pascal:
1) поддержка типов данных: integer, real, boolean, string
2) полиморфные read и wtire
3) отсутствие приведения типов
4) все переменные обязаны быть инициализированы сразу при объявлении
5) пользовательские функции (без процедур) от n аргументов
6) у функций может быть только один return в самом конце
7) поддерживает if
8) поддерживается while



список команд
stack run -- help

вывод ппринта в терминал
stack run -- pprint example.pas

вывод ппринта прямо в файл
stack run -- format example.pas

проверить файл на ошибки
stack run -- check example.pas

запустить
stack run -- run example.pas