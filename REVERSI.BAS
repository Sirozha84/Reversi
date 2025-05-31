DECLARE SUB Game (size!, m!(), pl1 AS STRING, pl2 AS STRING, move!, gr!())
DECLARE SUB NewGame ()
DECLARE SUB Init ()
DECLARE SUB Load ()
DECLARE SUB LoadGRP (m!(), file AS STRING, cn!, ln!)

REDIM SHARED grf(0, 0)   'Фишки                40*32*2
REDIM SHARED grf8(0, 0)  'Фишки для поля 8x8   40*32*9
REDIM SHARED grf16(0, 0) 'Фишки для поля 16x16 20*16*9
REDIM SHARED grf32(0, 0) 'Фишки для поля 32x32 10*12*9
REDIM SHARED grc(0, 0)   'Цветовые градации    20*16*56
REDIM SHARED grk(0, 0)   'Кнопки меню          200*25*14
REDIM SHARED gra(0, 0)   'Стрелки меню         46*24*11
REDIM SHARED grn(0, 0)   'Буквы названия       46*46*7
REDIM SHARED grp(0, 0)   'Полоса прокрутки     20*16*6
REDIM SHARED grt(0, 0)   'Табло                94*322*1

Init
NewGame

SUB Game (size, m(), pl1 AS STRING, pl2 AS STRING, move, gr())
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

'LINE (100, 0)-(539, 321), 14, BF
    PUT (0, 0), grt(0, 0), PSET
    PUT (546, 0), grt(0, 0), PSET

'Рисуем поле
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            PUT (left + x * wd, bott + y * hg), gr(0, m(x, y)), PSET
        NEXT
    NEXT


SLEEP

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
    size = 8
    REDIM m(size - 1, size - 1)
    m(size / 2, size / 2) = 1
    m(size / 2 - 1, size / 2 - 1) = 1
    m(size / 2, size / 2 - 1) = 2
    m(size / 2 - 1, size / 2) = 2
    DIM p1 AS STRING
    DIM p2 AS STRING
    p1 = "Сергей"
    p2 = "Computer"
    SELECT CASE size
        CASE 8:
            Game size, m(), p1, p2, 1, grf8()
        CASE 16:
            Game size, m(), p1, p2, 1, grf16()
        CASE 32:
            Game size, m(), p1, p2, 1, grf32()
    END SELECT
END SUB

