DECLARE SUB Game (gr!(), move!)
DECLARE FUNCTION StepComputer! (pl!)
DECLARE SUB CondidatesCalc (pl!)
DECLARE SUB StepEnd (cx!, cy!, pl!)
DECLARE SUB CellFill (cx!, cy!, mx!, my!, pl!)
DECLARE FUNCTION CellCheck! (x!, y!, mx!, my!, pl!)
DECLARE SUB CondidateCalc (pl!)
DECLARE FUNCTION StepHuman! (gr!(), col!)
DECLARE SUB NewGame ()
DECLARE SUB Init ()
DECLARE SUB Load ()
DECLARE SUB LoadGRP (m!(), file AS STRING, cn!, ln!)

DIM SHARED kup$: kup$ = CHR$(0) + CHR$(75)
DIM SHARED kdn$: kdn$ = CHR$(0) + CHR$(77)
DIM SHARED klf$: klf$ = CHR$(0) + CHR$(80)
DIM SHARED krt$: krt$ = CHR$(0) + CHR$(72)
DIM SHARED ken$: ken$ = CHR$(13)
DIM SHARED kes$: kes$ = CHR$(27)

REDIM SHARED grf(0, 0)  'Фишки                40*32*2
REDIM SHARED grf8(0, 0) 'Фишки для поля 8x8   40*32*9
REDIM SHARED grf16(0, 0)'Фишки для поля 16x16 20*16*9
REDIM SHARED grf32(0, 0)'Фишки для поля 32x32 10*12*9
REDIM SHARED grc(0, 0)  'Цветовые градации    20*16*56
REDIM SHARED grk(0, 0)  'Кнопки меню          200*25*14
REDIM SHARED gra(0, 0)  'Стрелки меню         46*24*11
REDIM SHARED grn(0, 0)  'Буквы названия       46*46*7
REDIM SHARED grp(0, 0)  'Полоса прокрутки     20*16*6
REDIM SHARED grt(0, 0)  'Табло                94*322*1

DIM SHARED m(31, 31)    'Игровое поле
DIM SHARED size         'Размер
DIM SHARED pl(2) AS STRING   'Имена игроков
DIM SHARED left
DIM SHARED bott
DIM SHARED wd
DIM SHARED hg
DIM SHARED kx           'Курсор
DIM SHARED ky
DIM SHARED c(31, 31)    'Матрица для вычисления кодидатов

Init
size = 8
NewGame

'Расч)т количества "обращённых" в клетке cx, cy
'mx, my - направление
'pl - игок
FUNCTION CellCheck (cx, cy, mx, my, pl)
    x = cx
    y = cy
    pr = 3 - pl
    c = 0
    ex = 0
    DO
        x = x + mx
        y = y + my
        IF x < 0 OR y < 0 OR x > size - 1 OR y > size - 1 THEN
            ex = -1
        ELSE
            IF m(x, y) = 0 THEN EXIT FUNCTION
            IF m(x, y) = pr THEN c = c + 1
            IF m(x, y) = pl THEN CellCheck = c: EXIT FUNCTION
        END IF
    LOOP WHILE NOT ex
    '
END FUNCTION

SUB CellFill (cx, cy, mx, my, pl)
    c = CellCheck(cx, cy, mx, my, pl)
    FOR i = 0 TO c
        m(cx + mx * i, cy + my * i) = pl
    NEXT
END SUB

'Вычисление матрицы кондидатов
'0 - Если хода нет, 1 и более - количество "обращённых" противников
SUB CondidatesCalc (pl)
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            IF m(x, y) <> 0 THEN
                c(x, y) = 0
            ELSE
                c = CellCheck(x, y, 0, 1, pl)
                c = c + CellCheck(x, y, 1, 1, pl)
                c = c + CellCheck(x, y, 1, 0, pl)
                c = c + CellCheck(x, y, 1, -1, pl)
                c = c + CellCheck(x, y, 0, -1, pl)
                c = c + CellCheck(x, y, -1, -1, pl)
                c = c + CellCheck(x, y, -1, 0, pl)
                c = c + CellCheck(x, y, -1, 1, pl)
                c(x, y) = c
            END IF
            'c(0, 2) = 10
            'LOCATE 9 - y, x * 3 + 1: PRINT c(x, y)
        NEXT
    NEXT
END SUB


SUB Game (gr(), move)
    SELECT CASE size
        CASE 8:
            left = 110
            bott = 1
            wd = 40 + 14
            hg = 32 + 9
        CASE 16:
            left = 107
            bott = 3
            wd = 20 + 7
            hg = 16 + 4
        CASE 32:
            left = 114
            bott = 2
            wd = 10 + 3
            hg = 8 + 2
    END SELECT

    'Рисуем панели
    PUT (0, 0), grt(0, 0), PSET
    PUT (546, 0), grt(0, 0), PSET
    COLOR 14
    LOCATE 10, 1
    PRINT pl(1)
    LOCATE 10, 69
    PRINT pl(2)

    endgame = 0
    DO

        'Рисуем поле
        FOR x = 0 TO size - 1
            FOR y = 0 TO size - 1
                PUT (left + x * wd, bott + y * hg), gr(0, m(x, y)), PSET
            NEXT
        NEXT

        'Определяем, кто сейчас ходит

        IF pl(move) <> "Computer" THEN
            'Ход делает человек
            s = StepHuman(gr(), move)
        ELSE
            'Ход делает компьютер
            s = StepComputer(move)
        END IF

        IF s = 2 THEN endgame = -1
        move = 3 - move

    LOOP WHILE NOT endgame


END SUB

SUB Init
    SCREEN 9
    CLS
    WINDOW (0, 0)-(639, 349)
    LoadGRP grf(), "f.grp", 2, 281
    LoadGRP grf8(), "f8.grp", 9, 281
    LoadGRP grf16(), "f16.grp", 9, 81
    LoadGRP grf32(), "f32.grp", 9, 21
    LoadGRP grc(), "color.grp", 99, 49
    LoadGRP grk(), "keys.grp", 14, 626
    LoadGRP gra(), "kursor.grp", 11, 145
    LoadGRP grn(), "name.grp", 7, 277
    LoadGRP grp(), "prok.grp", 6, 49
    LoadGRP grt(), "tab.grp", 1, 3865
END SUB

'Загрузка спрайтов
'm() - массивЁ содержащий спрайт
'file - имя файла
'cn - колиочество спрайтов
'ln - длина массива
SUB LoadGRP (m(), file AS STRING, cn, ln)
    REDIM m(ln - 1, cn - 1)
    OPEN file FOR RANDOM AS 1 LEN = 4
    i = 1
    FOR n = 0 TO cn - 1
        FOR d = 0 TO ln - 1
            GET #1, i, m(d, n)
            i = i + 1
        NEXT
    NEXT
    CLOSE
END SUB

SUB NewGame

    'Подготовка поля
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            m(x, y) = 0
        NEXT
    NEXT
    m(size / 2, size / 2) = 1
    m(size / 2 - 1, size / 2 - 1) = 1
    m(size / 2, size / 2 - 1) = 2
    m(size / 2 - 1, size / 2) = 2

    'Поодготовка игроков
    pl(1) = "Сергей"
    pl(2) = "Computer"
    move = 1
    kx = 2
    ky = 4

    'Запуск нужного игры
    SELECT CASE size
        CASE 8:
            Game grf8(), move
        CASE 16:
            Game grf16(), move
        CASE 32:
            Game grf32(), move
    END SELECT

END SUB

FUNCTION StepComputer (pl)
    CondidatesCalc pl
    max = 0
    cx = 0
    cy = 0
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            IF c(x, y) > max THEN
                max = c(x, y)
                cx = x
                cy = y
            END IF
        NEXT
    NEXT
    IF max > 0 THEN
        StepEnd cx, cy, pl
    ELSE
        StepComputer = 1
    END IF
END FUNCTION

'Завершение хода - обращение фишек противника
SUB StepEnd (cx, cy, pl)
    CellFill cx, cy, 0, 1, pl
    CellFill cx, cy, 1, 1, pl
    CellFill cx, cy, 1, 0, pl
    CellFill cx, cy, 1, -1, pl
    CellFill cx, cy, 0, -1, pl
    CellFill cx, cy, -1, -1, pl
    CellFill cx, cy, -1, 0, pl
    CellFill cx, cy, -1, 1, pl
END SUB

'Ход человека 
'0 - ход сделан
'1 - пас
'2 - выход
FUNCTION StepHuman (gr(), pl)
    CondidatesCalc pl
    DO
        x = left + kx * wd
        y = bott + ky * hg

        IF c(kx, ky) > 0 THEN
            PUT (x, y), gr(0, 5), AND
            PUT (x, y), gr(0, 2 + pl), XOR
        ELSE
            PUT (x, y), gr(0, 8), AND
            PUT (x, y), gr(0, 5 + pl), XOR
        END IF
        DO
            k$ = INKEY$
        LOOP WHILE k$ = ""
    
        PUT (left + kx * wd, bott + ky * hg), gr(0, m(kx, ky)), PSET
        IF k$ = kup$ AND kx > 0 THEN kx = kx - 1
        IF k$ = kdn$ AND kx < size - 1 THEN kx = kx + 1
        IF k$ = klf$ AND ky > 0 THEN ky = ky - 1
        IF k$ = krt$ AND ky < size - 1 THEN ky = ky + 1
        IF k$ = ken$ AND c(kx, ky) > 0 THEN
            StepEnd kx, ky, pl
            EXIT FUNCTION
        END IF
    LOOP UNTIL k$ = " " OR k$ = kes$
    IF k$ = " " THEN StepHuman = 1
    IF k$ = kes$ THEN StepHuman = 2
END FUNCTION

