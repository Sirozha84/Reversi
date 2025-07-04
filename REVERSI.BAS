DECLARE SUB Intro ()
DECLARE SUB Square (x!, y!, n!, i!)
DECLARE SUB Instruction ()
DECLARE SUB PlaySound (i!)
DECLARE SUB Options ()
DECLARE SUB Game (gr!())
DECLARE SUB SaveGame ()
DECLARE SUB LoadGame ()
DECLARE SUB InitGame (mode!, lvl!, sz!)
DECLARE SUB SaveParams ()
DECLARE SUB ShowHiScores ()
DECLARE SUB Save ()
DECLARE FUNCTION ScoreString$ (h!, i!)
DECLARE FUNCTION ScoreColor! (i!)
DECLARE SUB RedWindow (rows!)
DECLARE SUB Win (w!)
DECLARE SUB DrawField (gr!())
DECLARE FUNCTION InputName! (m!, pl!)
DECLARE FUNCTION Menu! ()
DECLARE FUNCTION MiniMenu! (menutype!)
DECLARE SUB DrawTable (pl!)
DECLARE SUB Benchmark ()
DECLARE SUB Delay (s!)
DECLARE FUNCTION WhoWin! (pass!)
DECLARE SUB CandidatesCalc (pl!)
DECLARE FUNCTION FCount! (pl!)
DECLARE FUNCTION StepComputer! (pl!)
DECLARE SUB CondidatesCalc (pl!)
DECLARE SUB StepEnd (cx!, cy!, pl!)
DECLARE SUB CellFill (cx!, cy!, mx!, my!, pl!)
DECLARE FUNCTION CellCheck! (x!, y!, mx!, my!, pl!)
DECLARE SUB CondidateCalc (pl!)
DECLARE FUNCTION StepHuman! (gr!(), col!)
DECLARE SUB Init ()
DECLARE SUB Load ()
DECLARE SUB LoadGRP (m!(), file AS STRING, cn!, ln!)

DIM SHARED kup$: kup$ = CHR$(0) + CHR$(72)
DIM SHARED kdn$: kdn$ = CHR$(0) + CHR$(80)
DIM SHARED klf$: klf$ = CHR$(0) + CHR$(75)
DIM SHARED krt$: krt$ = CHR$(0) + CHR$(77)
DIM SHARED kbs$: kbs$ = CHR$(8)
DIM SHARED ken$: ken$ = CHR$(13)
DIM SHARED kes$: kes$ = CHR$(27)

REDIM SHARED grf(0, 0)  '��誨                40*32*2
REDIM SHARED grf8(0, 0) '��誨 ��� ���� 8x8   40*32*9
REDIM SHARED grf16(0, 0)'��誨 ��� ���� 16x16 20*16*9
REDIM SHARED grf32(0, 0)'��誨 ��� ���� 32x32 10*12*9
REDIM SHARED grc(0, 0)  '���⮢� �ࠤ�樨    20*16*56
REDIM SHARED grk(0, 0)  '������ ����          200*25*14
REDIM SHARED gra(0, 0)  '��५�� ����         46*24*11
REDIM SHARED grn(0, 0)  '�㪢� ��������       46*46*7
REDIM SHARED grp(0, 0)  '����� �ப��⪨     20*16*6
REDIM SHARED grt(0, 0)  '�����                94*322*1

DIM SHARED HiScores(5, 10) AS STRING
DIM SHARED FirstPlayer
DIM SHARED SoundEnable

DIM SHARED m(31, 31)    '��஢�� ����
DIM SHARED nm(31, 31)   '����� ���� ��� ����������
DIM SHARED pl(1) AS STRING '����� ��ப��
DIM SHARED move         '����騩 室
DIM SHARED level        '�஢��� ᫮�����
DIM SHARED size         '������
DIM SHARED score(1)     '�窨 (����)
DIM SHARED left         '���न���� ����
DIM SHARED bott
DIM SHARED wd           '�����ﭨ� ����� ���⪠��
DIM SHARED hg
DIM SHARED kx           '�����
DIM SHARED ky
DIM SHARED c(31, 31)    '����� ��� ���᫥��� ������⮢
DIM SHARED cicles       '������ ����
DIM SHARED kmm          '����� �������� ����
DIM SHARED IsLoadGame   '���� "�� ��� ����㦥����"
DIM SHARED NeedRefresh  '���� "�㦭� ����ᮢ��� ������� ����"

DATA 1,1,1,1,0,0,1,1,1,1
DATA 0,0,0,1,0,0,1,0,0,1
DATA 1,1,1,1,0,0,1,0,1,1
DATA 1,0,0,0,0,0,1,0,0,0
DATA 1,1,1,1,0,0,1,1,1,1

Init
'pl(1) = "Computer": InitGame 0, 1, 8: END
Intro
DO
LOOP UNTIL Menu

SUB Benchmark
    old = 0
    c = 0
    DO
        cicles = 0
        t = TIMER
        DO
            FOR j = 0 TO 99
                s = SIN(0)
            NEXT
            cicles = cicles + 1
        LOOP UNTIL t <> TIMER
        IF ABS(old - cicles) < 2 THEN c = c + 1 ELSE c = 0
        old = cicles
    LOOP WHILE c < 2
END SUB

'���᫥��� ������ �������⮢
'0 - �᫨ 室� ���, 1 � ����� - ������⢮ "�������" ��⨢�����
SUB CandidatesCalc (pl)
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            c(x, y) = 0
            IF m(x, y) = -1 THEN
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
        NEXT
    NEXT
END SUB

'������ ������⢠ "�������" � ���⪥ cx, cy
'mx, my - ���ࠢ�����
'pl - ��ப
FUNCTION CellCheck (cx, cy, mx, my, pl)
    x = cx
    y = cy
    pr = 1 - pl
    c = 0
    ex = 0
    DO
        x = x + mx
        y = y + my
        IF x < 0 OR y < 0 OR x > size - 1 OR y > size - 1 THEN
            c = 0
            ex = -1
        ELSEIF m(x, y) = -1 THEN
            c = 0
            ex = -1
        ELSEIF m(x, y) = pr THEN
            c = c + 1
        ELSEIF m(x, y) = pl THEN
            ex = -1
        END IF
    LOOP UNTIL ex
    CellCheck = c
END FUNCTION

'����᪠ �㦨� �襪 �� �����
SUB CellFill (cx, cy, mx, my, pl)
    c = CellCheck(cx, cy, mx, my, pl)
    FOR i = 0 TO c
        x = cx + mx * i
        y = cy + my * i
        m(x, y) = pl
    NEXT
END SUB

'��㧠 � ᥪ㭤��
SUB Delay (s)
    FOR i = 0 TO s * cicles * 18
        FOR j = 0 TO 99
            sn = SIN(0)
        NEXT
        t = TIMER
    NEXT
END SUB

SUB DrawField (gr())

    '���㥬 ������
    SCREEN 9, 0, 1, 0
    CLS
    LOCATE 25, 29
    COLOR 7
    PRINT "�஡�� - ���, Esc - ����"
    FOR i = -226 TO 544 STEP 2
        LINE (i, 14)-(i + 321, 335), 8
    NEXT
    FOR i = 0 TO 1
        PUT (i * 546, 14), grt(0, 0), PSET
        PUT (27 + i * 546, 277), grf(0, i), PSET
    NEXT

    COLOR 14
    FOR i = 0 TO 1
        LOCATE 9, 7 + i * 68 - LEN(pl(i)) / 2
        PRINT pl(i)
    NEXT

    '���㥬 ����
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            PUT (left + x * wd, bott + y * hg), gr(0, m(x, y) + 1), PSET
        NEXT
    NEXT
    SCREEN 9, 0, 0, 0
    PCOPY 1, 0

END SUB

'����ᮢ�� ������ ⠡��
SUB DrawTable (pl)
    k = 94 / size / size
    COLOR 11
    FOR i = 0 TO 1
        p = 1 - i
        s = FCount(i)
        a = FCount(1 - i)
        LOCATE 13, 4 + i * 68
        PRINT s
        LOCATE 17, 4 + i * 68
        score(i) = s - a
        PRINT score(i)
        x = i * 546
        LINE (x, 42)-(x + 93, 55), 15, B
        LINE (x, 42)-(x + s * k, 55), 11 - (1 + i), BF
        LINE (x + s * k, 42)-(x + (s + a) * k, 55), 11 - (2 - i), BF
    NEXT
    COLOR 10 - pl
    LOCATE 1, 1
    PRINT TAB(INT(41 - (LEN(pl(pl)) + 7) / 2));
    PRINT "�����: " + pl(pl); TAB(80);
END SUB

'������� ������⢠ �襪
FUNCTION FCount (pl)
    cn = 0
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            IF m(x, y) = pl THEN cn = cn + 1
        NEXT
    NEXT
    FCount = cn
END FUNCTION

'��楤�� ����
SUB Game (gr())

    '��।��塞 ࠧ���� ����� � �襪
    SELECT CASE size
        CASE 8:
            left = 110
            bott = 15
            wd = 54
            hg = 41
        CASE 16:
            left = 100
            bott = 17
            wd = 28
            hg = 20
        CASE 32:
            left = 98
            bott = 16
            wd = 14
            hg = 10
    END SELECT

    DrawField gr()

    '��஢�� 横�
    pass = 0
    endgame = 0
    DO
        '������塞 ⠡��
        DrawTable move

        '��।��塞, �� ᥩ�� 室��
        IF pl(move) <> "Computer" THEN
            s = StepHuman(gr(), move)
        ELSE
            s = StepComputer(move)
        END IF
        IF s = 1 THEN pass = pass + 1 ELSE pass = 0 '��� ���
        IF s = 2 THEN endgame = -1                  '����� ��室�

        '������塞 ����
        FOR x = 0 TO size - 1
            FOR y = 0 TO size - 1
                IF first OR nm(x, y) <> m(x, y) THEN
                    nm(x, y) = m(x, y)
                    PUT (left + x * wd, bott + y * hg), gr(0, m(x, y) + 1), PSET
                END IF
            NEXT
        NEXT
        '���
        IF s = 0 THEN PlaySound 0
        IF s = 1 THEN PlaySound 1
        IF s = 2 THEN PlaySound 1
        
        '��।��塞, ���� �� ������
        w = WhoWin(pass > 1)
        IF w <> 2 THEN
            DrawTable move
            Win (w)
            endgame = -1
        END IF

        '���室 室�
        move = 1 - move

    LOOP WHILE NOT endgame
    NeedRefresh = -1
END SUB

'���樠������, ����㧪� 䠩��� ��䨪�
SUB Init
    SCREEN 9
    COLOR 8
    LOCATE 25, 70
    PRINT "����㧪�..."
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
    FirstPlayer = 0
    SoundEnable = 1
    OPEN "reversi.cfg" FOR APPEND AS 1
    CLOSE
    OPEN "reversi.cfg" FOR INPUT AS 1
        IF NOT EOF(1) THEN INPUT #1, FirstPlayer
        IF NOT EOF(1) THEN INPUT #1, SoundEnable
    CLOSE

    OPEN "hiscores.sav" FOR APPEND AS 1
    CLOSE

    OPEN "hiscores.sav" FOR INPUT AS 1
    FOR i = 0 TO 5
        FOR j = 0 TO 9
            IF NOT EOF(1) THEN INPUT #1, s$ ELSE s$ = ""
            IF LEN(s$) = 39 THEN
                HiScores(i, j) = s$
            ELSE
                HiScores(i, j) = "......................................."
            END IF
        NEXT
    NEXT
    CLOSE
    kmm = 6
    NeedRefresh = -1
    Benchmark
    CLS
    Delay .5
END SUB

'����� ����� ����
SUB InitGame (mode, lvl, sz)

    IF mode = 0 THEN
        level = lvl
        size = sz
    
        '�����⮢�� ����
        FOR x = 0 TO size - 1
            FOR y = 0 TO size - 1
                m(x, y) = -1
            NEXT
        NEXT
        m(size / 2, size / 2) = 0
        m(size / 2 - 1, size / 2 - 1) = 0
        m(size / 2, size / 2 - 1) = 1
        m(size / 2 - 1, size / 2) = 1
        FOR x = 0 TO size - 1
            FOR y = 0 TO size - 1
                nm(x, y) = m(x, y)
            NEXT
        NEXT

        '�����⮢�� ��ப��
        move = 0
        kx = size / 2 - 2
        ky = size / 2
        IsLoadGame = 0
    ELSE
        LoadGame
        IF size = 0 THEN
            RedWindow 3
            LOCATE 13, 27
            COLOR 11
            PRINT "���࠭���� ���� �� �����㦥��"
            DO: LOOP WHILE INKEY$ <> ""
            DO: LOOP WHILE INKEY$ = ""
            EXIT SUB
        END IF
        IsLoadGame = -1
    END IF

    '����� �㦭��� ����
    SELECT CASE size
        CASE 8
            Game grf8()
        CASE 16
            Game grf16()
        CASE 32
            Game grf32()
    END SELECT

END SUB

FUNCTION InputName (m, pl)
    RedWindow 7
    PUT (300, 186), grf(0, pl), PSET
    LOCATE 13, 26
    COLOR 11
    IF m = 1 THEN
        PRINT "       ������ ᢮� ���"
    ELSE
        IF pl = 1 THEN
            PRINT "����";
        ELSE
            PRINT "��ன";
        END IF
        PRINT " ��ப, ������ ᢮� ���"
    END IF
    LOCATE 15, 27
    COLOR 1
    PRINT "�  �  �  �  �  �  �  �  �  �";
    k = 0
    DIM l$(9)
    a = 0
    DO
        DO
            FOR i = 0 TO k - 1
                LOCATE 15, 27 + i * 3
                SELECT CASE INT(a + i) MOD 4
                    CASE 0: COLOR 2
                    CASE 1: COLOR 2
                    CASE 2: COLOR 10
                    CASE 3: COLOR 10
                END SELECT
                PRINT l$(i)
            NEXT
            SELECT CASE INT(a)
                CASE 0: COLOR 0
                CASE 1: COLOR 5
                CASE 2: COLOR 13
                CASE 3: COLOR 5
            END SELECT
            c = k
            IF c > 9 THEN c = 9
            LOCATE 14, 25 + c * 3
            PRINT "���Ŀ"
            LOCATE 16, 25 + c * 3
            PRINT "�����"
            a = a + .2
            IF a >= 4 THEN a = 0
            k$ = INKEY$
            Delay .01
        LOOP WHILE k$ = ""
        LOCATE 14, 25 + k * 3
        PRINT "     "
        LOCATE 16, 25 + k * 3
        PRINT "     "
        IF (k$ >= "a" AND k$ <= "z") OR (k$ >= "A" AND k$ <= "Z") THEN
            l$(c) = k$
            k = k + 1
            IF k > 10 THEN k = 10
        END IF
        IF k$ = kbs$ THEN
            k = k - 1
            IF k < 0 THEN k = 0
            LOCATE 15, 27 + k * 3
            COLOR 1
            PRINT "�"
        END IF
        IF k$ = kes$ THEN
            k = -1
            k$ = ken$
        END IF
    LOOP UNTIL k$ = ken$
    PCOPY 1, 0
    IF k > 0 THEN
        pl(pl) = ""
        FOR i = 0 TO k - 1
            pl(pl) = pl(pl) + l$(i)
        NEXT
        InputName = 0
    ELSEIF k = 0 THEN
        pl(pl) = "Player" + STR$(pl + 1)
    ELSE
        InputName = -1
    END IF
END FUNCTION

SUB Instruction
    RedWindow 15
    s$ = "                                          "
    p = 0
    DO
        LOCATE 6, 35
        COLOR 11
        PRINT "R E V E R S I"
        COLOR 13
        SELECT CASE p
            CASE 0
                LOCATE 9, 20: PRINT s$
                LOCATE 10, 20
                PRINT "�����:                   1.1 (21.06.2025)"
                LOCATE 11, 20: PRINT s$
                LOCATE 12, 20
                PRINT "����:                      ��ࣥ� ��थ��"
                LOCATE 13, 20: PRINT s$
                LOCATE 14, 20
                PRINT "����:                       sg-software.ru"
                LOCATE 15, 20: PRINT s$
                LOCATE 16, 20
                PRINT "��㯯� � ��:             vk.com/sgsoftware"
                LOCATE 17, 20: PRINT s$
                
            CASE 1
                LOCATE 9, 20
                PRINT "������ -  ���⮫쭠� ��� ���� ��ப�� ��"
                LOCATE 10, 20
                PRINT "�����⭮� ����. ��ப� �� ��।� ���⠢-"
                LOCATE 11, 20
                PRINT "���� �誨  ᢮���  梥�  ⠪��  ��ࠧ��,"
                LOCATE 12, 20
                PRINT "�⮡�  ����� ����� �誮� � 㦥  ����饩��"
                LOCATE 13, 20
                PRINT "� �� �� ��ਧ��⠫�, ���⨪��� ��� �����-"
                LOCATE 14, 20
                PRINT "����  ��室�����  �誨  ᮯ�୨��.  �����"
                LOCATE 15, 20
                PRINT "�誨  ���� ��墠祭� �  ���饭� � ᢮�."
                LOCATE 16, 20
                PRINT "��������� �� ��ப,  �  ���ண� � ������"
                LOCATE 17, 20
                PRINT "��墠� �ᥣ� ����  �襪 �������� �����."
            CASE 2
                LOCATE 9, 20
                PRINT "������ �뢠�� ���樨,  ����� ��� �����-"
                LOCATE 10, 20
                PRINT "��� 室��, ⮣�� �����  ᤥ���� ���.  �᫨"
                LOCATE 11, 20
                PRINT "���  ������  ��� ��ப�  -  ��� ��⠥���"
                LOCATE 12, 20
                PRINT "������񭭮�.                              "
                LOCATE 13, 20: PRINT s$
                LOCATE 14, 20
                PRINT "                ��ࠢ�����                "
                LOCATE 15, 20
                PRINT "�롮� ���⪨:                      ��५��"
                LOCATE 16, 20
                PRINT "������� 室:                         Enter"
                LOCATE 17, 20
                PRINT "���:                                �஡��"
        END SELECT
        LOCATE 20, 34
        COLOR 3
        PRINT "��࠭��:"; p + 1; "/ 3"

        '����� �ப��⪨
        PUT (500, 63), grp(0, 0), PSET
        PUT (500, 271), grp(0, 1), PSET
        FOR i = 79 TO 260 STEP 16
            PUT (500, i), grp(0, 5), PSET
        NEXT
        PUT (500, 255 - p * 88), grp(0, 4), PSET

        DO: LOOP WHILE INKEY$ <> ""
        oldp = p
        DO
            k$ = INKEY$
            IF k$ = kup$ AND p > 0 THEN p = p - 1
            IF k$ = kdn$ AND p < 2 THEN p = p + 1
        LOOP WHILE k$ <> ken$ AND k$ <> kes$ AND oldp = p
        IF p > oldp THEN
            PUT (500, 63), grp(0, 2), PSET
            Delay .1
        END IF
        IF p < oldp THEN
            PUT (500, 271), grp(0, 3), PSET
            Delay .1
        END IF
    LOOP WHILE k$ <> ken$ AND k$ <> kes$
END SUB

SUB Intro
    d = .05
    DIM sg(9, 4)
    
    FOR y = 0 TO 4
        FOR x = 0 TO 9
            READ sg(x, y)
        NEXT
    NEXT
    CLS

    'Red Square
    FOR i = 0 TO 7
        FOR x = 1 TO 5
            FOR y = 10 TO 14
                Square x, y, 0, i
            NEXT
        NEXT
        Delay d
    NEXT

    'Green Square
    FOR i = 0 TO 7
        FOR x = 4 TO 8
            FOR y = 9 TO 13
                IF x < 6 AND y > 9 THEN
                    Square x, y, 1, i
                ELSE
                    Square x, y, 6, i
                END IF
            NEXT
        NEXT
        Delay d
    NEXT

    'Blue Square
    FOR i = 0 TO 7
        FOR x = 2 TO 6
            FOR y = 7 TO 11
                IF x < 4 AND y > 9 THEN
                    Square x, y, 3, i
                ELSEIF x > 3 AND y > 8 THEN
                    Square x, y, 2, i
                ELSE
                    Square x, y, 4, i
                END IF
            NEXT
        NEXT
        Delay d
    NEXT

    'SG
    FOR i = 0 TO 15
        FOR x = 0 TO 9
            FOR y = 0 TO 4
                b = x - y - 2
                IF b > 0 THEN b = 1 - b
                IF b < 5 THEN ii = i + b ELSE ii = b - i
                IF sg(x, y) = 1 AND ii > 0 AND ii < 16 THEN Square x, y, 4, ii
            NEXT
        NEXT
        Delay d
    NEXT

    Delay 1.5

    '��ન
    FOR i = 0 TO 175
        LINE (0, i)-(639, i), 0
        LINE (0, 349 - i)-(639, 349 - i), 0
        Delay .005
    NEXT

    '�।�⠢���...
    p$ = "� � � � � � � � � � � �"
    LOCATE 13, 29
    COLOR 2
    PRINT p$
    Delay .2

    LOCATE 13, 29
    COLOR 10
    PRINT p$
    Delay 1.5

    LOCATE 13, 29
    COLOR 2
    PRINT p$
    Delay .2

    CLS
    Delay .2

    '���񫥭쪮� ������ ��������
    SCREEN 9, 0, 1, 0
    CLS
    FOR a = 0 TO 100
        CLS
        FOR i = 0 TO 6
            y = ABS(SIN((a + i) / 5)) * (100 - a)
            PUT (27 + i * 90, 152 + y), grn(0, i), PSET
        NEXT
        LINE (0, 152)-(319 - a * 10, 349), 0, BF
        LINE (639, 152)-(320 + a * 10, 349), 0, BF
        PCOPY 1, 0
        Delay .01
    NEXT
    SCREEN 9, 0, 0, 0

    '��஫� ������ �����
    FOR y = 152 TO 280 STEP 3
        FOR i = 0 TO 6
            PUT (27 + i * 90, y), grn(0, i), PSET
        NEXT
        LINE (0, y - 1)-(639, y - 4), 0, BF
        Delay .001
    NEXT


END SUB

SUB LoadGame
    OPEN "game.sav" FOR RANDOM AS 1 LEN = 4
        r = 1
        FOR i = 0 TO 1
            GET #1, r, l
            r = r + 1
            pl(i) = ""
            FOR j = 1 TO l
                GET #1, r, b
                pl(i) = pl(i) + CHR$(b)
                r = r + 1
            NEXT
        NEXT
        GET #1, r, move
        r = r + 1
        GET #1, r, size
        r = r + 1
        FOR i = 0 TO size - 1
            FOR j = 0 TO size - 1
                GET #1, r, m(i, j)
                r = r + 1
                nm(i, j) = m(i, j)
            NEXT
        NEXT
        GET #1, r, kx
        r = r + 1
        GET #1, r, ky
        r = r + 1
    CLOSE
END SUB

'����㧪� �ࠩ⮢
'm() - ���ᨢ, ᮤ�ঠ騩 �ࠩ�
'file - ��� 䠩��
'cn - ������⢮ �ࠩ⮢
'ln - ����� ���ᨢ�
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

FUNCTION Menu
    IF NeedRefresh THEN
        CLS
        LOCATE 25, 76
        COLOR 8
        PRINT "V 1.1"
        COLOR 15
        FOR i = 0 TO 6
            PUT (27 + i * 90, 280), grn(0, i), PSET
        NEXT

        FOR i = 0 TO 9
            y = i * .6
            LINE (100 + i, 10 + y)-(100 + i, 239 - y)
            LINE -(539 - i, 239 - y)
            LINE -(539 - i, 10 + y), 8
            LINE -(100 + i, 10 + y), 8
        NEXT
        LINE (110, 16)-(529, 233), 7, BF
        FOR i = 0 TO 6
            PUT (220, 22 + i * 30), grk(0, i), PSET
        NEXT
    ELSE
        PCOPY 1, 0
    END IF
    DO: LOOP UNTIL INKEY$ = ""
    DO
        PUT (140, 22 + kmm * 30), gra(0, INT(a)), PSET
        PUT (453, 22 + kmm * 30), gra(0, INT(a) + 5), PSET
        a = a + .2
        IF a >= 5 THEN a = 0
        Delay .01
        k$ = INKEY$
        IF k$ <> "" THEN
            LINE (140, 22 + kmm * 30)-(185, 45 + kmm * 30), 7, BF
            LINE (453, 22 + kmm * 30)-(498, 45 + kmm * 30), 7, BF
        END IF
        IF k$ = kup$ THEN kmm = kmm + 1
        IF k$ = kdn$ THEN kmm = kmm - 1
        IF kmm > 6 THEN kmm = 0
        IF kmm < 0 THEN kmm = 6
        IF k$ = kes$ THEN k$ = ken$: kmm = 0
    LOOP UNTIL k$ = ken$
    PUT (220, 22 + kmm * 30), grk(0, kmm + 7), PSET
    Delay .15
    PUT (220, 22 + kmm * 30), grk(0, kmm), PSET
    NeedRefresh = 0

    '����� ���
    IF kmm = 6 THEN
        pl(1 - FirstPlayer) = "Computer"
        k = InputName(1, FirstPlayer)
        IF k <> -1 THEN k = MiniMenu(1)
        IF k <> -1 THEN
            s = 8
            IF k = 1 THEN s = 16
            IF k = 2 THEN s = 32
            k = MiniMenu(2)
        END IF
        IF k <> -1 THEN
            l = 0
            IF k = 1 THEN l = 1
            InitGame 0, l, s
        END IF
    END IF

    '����� ��� ��� ���� ��ப��
    IF kmm = 5 THEN
        k = InputName(2, 0)
        IF k <> -1 THEN k = InputName(2, 1)
        IF k <> -1 THEN k = MiniMenu(1)
        IF k <> -1 THEN
            s = 8
            IF k = 1 THEN s = 16
            IF k = 2 THEN s = 32
            InitGame 0, l, s
        END IF
    END IF

    '��稥 �㭪�� ����
    IF kmm = 4 THEN ShowHiScores
    IF kmm = 3 THEN InitGame 1, 0, 0
    IF kmm = 2 THEN Options
    IF kmm = 1 THEN Instruction

    Menu = (kmm = 0)
END FUNCTION

FUNCTION MiniMenu (menutype)
    DIM p(2) AS STRING
    SELECT CASE menutype
        CASE 0
            p(0) = " �த������ (Esc)"
            p(1) = "���࠭��� � ���"
            p(2) = "      ���"
            max = 2
            ex = 0
        CASE 1
            tit$ = "   �롥�� ࠧ��� ����"
            p(0) = "      8 x 8"
            p(1) = "     16 x 16"
            p(2) = "     32 x 32"
            max = 2
            ex = -1
        CASE 2
            tit$ = "�롥�� �஢��� ᫮�����"
            p(0) = "    ��ଠ���"
            p(1) = "     ������"
            max = 1
            ex = -1
    END SELECT
    rows = max + 1
    sh = 0
    IF tit$ <> "" THEN sh = 1   '�����, �᫨ ���� ⠩�
    rows = rows + sh
    RedWindow rows
    top = 13 - INT(rows / 2)
    DO
    LOOP UNTIL INKEY$ = ""
    k = 0
    DO
        LOCATE top, 28
        COLOR 11
        PRINT tit$
        FOR i = 0 TO max
            LOCATE top + i + sh, 32
            COLOR 9
            IF k = i THEN COLOR 13
            PRINT p(i)
        NEXT
        DO
            k$ = INKEY$
        LOOP WHILE k$ = ""
        IF k$ = kup$ THEN k = k - 1
        IF k$ = kdn$ THEN k = k + 1
        IF k < 0 THEN k = max
        IF k > max THEN k = 0
        IF k$ = kes$ THEN k = ex: k$ = ken$
    LOOP UNTIL k$ = ken$
    PCOPY 1, 0
    MiniMenu = k
END FUNCTION

SUB Options
    RedWindow 3

    DIM p(1) AS STRING
    p(0) = "���� 室��: "
    p(1) = "        ���: "
    DIM pp(1, 1) AS STRING
    pp(0, 0) = "��ப    "
    pp(0, 1) = "��������"
    pp(1, 0) = "�몫"
    pp(1, 1) = "��� "
    DIM o(1)
    o(0) = FirstPlayer
    o(1) = SoundEnable
    DO: LOOP UNTIL INKEY$ = ""
    k = 0
    DO
        LOCATE 12, 38
        COLOR 11
        PRINT "��樨"
        FOR i = 0 TO 1
            LOCATE 13 + i, 30
            COLOR 9
            IF k = i THEN COLOR 13
            PRINT p(i); pp(i, o(i))
        NEXT
        DO
            k$ = INKEY$
        LOOP WHILE k$ = ""
        IF k$ = kup$ THEN k = k - 1
        IF k$ = kdn$ THEN k = k + 1
        IF k < 0 THEN k = 1
        IF k > 1 THEN k = 0
        IF k$ = klf$ OR k$ = krt$ THEN o(k) = 1 - o(k)
        IF k$ = ken$ OR k$ = kes$ THEN
            FirstPlayer = o(0)
            SoundEnable = o(1)
            SaveParams
            k$ = kes$
        END IF
    LOOP UNTIL k$ = ken$ OR k$ = kes$
END SUB

SUB PlaySound (n)
    IF n = 0 THEN
        IF SoundEnable = 1 THEN
            i = 1
            DO
                SOUND 50, i
                SOUND 100, i
                SOUND 200, i
                i = i / 2

            LOOP WHILE i > .01
        ELSE
            Delay .3
        END IF
    END IF
    IF n = 1 THEN
        IF SoundEnable = 1 THEN
            FOR i = 200 TO 50 STEP -50
                SOUND i * 4, .5
                SOUND i * 2, .5
                SOUND i, .5
            NEXT
        ELSE
            Delay .5
        END IF
    END IF
    IF n = 2 THEN
        IF SoundEnable = 1 THEN
            FOR i = 50 TO 800 STEP 50
                SOUND i, .5
                SOUND i * 2, .5
                SOUND i * 4, .5
            NEXT
        END IF
    END IF
END SUB

SUB RedWindow (rows)
    top = 13 - INT(rows / 2)
    x = 140
    y = 350 - (top + rows - 1) * 14 - 7
    h = (rows) * 14 - 1 + 14
    PCOPY 0, 1
    FOR i = 1 TO -.01 STEP -.1
        x1 = x + i * 170
        y1 = y + i * h / 2
        x2 = 639 - x - i * 170
        y2 = y + h - i * h / 2
        LINE (x1, y1)-(x2, y2), 13, B
        LINE (x1 + 1, y1 + 1)-(x2 - 1, y2 - 1), 13, B
        LINE (x1 + 2, y1 + 2)-(x2 - 2, y2 - 2), 12, B
        LINE (x1 + 3, y1 + 3)-(x2 - 3, y2 - 3), 12, B
        LINE (x1 + 4, y1 + 4)-(x2 - 4, y2 - 4), 4, B
        LINE (x1 + 5, y1 + 5)-(x2 - 5, y2 - 5), 4, B
        LINE (x1 + 6, y1 + 6)-(x2 - 6, y2 - 6), 0, BF
        Delay .01
    NEXT
END SUB

SUB SaveGame
    OPEN "game.sav" FOR RANDOM AS 1 LEN = 4
        r = 1
        FOR i = 0 TO 1
            l = LEN(pl(i))
            PUT #1, r, l
            r = r + 1
            FOR j = 1 TO LEN(pl(i))
                b = ASC(MID$(pl(i), j, 1))
                PUT #1, r, b
                r = r + 1
            NEXT
        NEXT
        PUT #1, r, move
        r = r + 1
        PUT #1, r, size
        r = r + 1
        FOR i = 0 TO size - 1
            FOR j = 0 TO size - 1
                PUT #1, r, m(i, j)
                r = r + 1
            NEXT
        NEXT
        PUT #1, r, kx
        r = r + 1
        PUT #1, r, ky
        r = r + 1
    CLOSE
END SUB

SUB SaveParams
    OPEN "reversi.cfg" FOR OUTPUT AS 1
        PRINT #1, FirstPlayer
        PRINT #1, SoundEnable
    CLOSE

    OPEN "hiscores.sav" FOR OUTPUT AS 1
        FOR i = 0 TO 5
            FOR j = 0 TO 9
                PRINT #1, HiScores(i, j)
            NEXT
        NEXT
    CLOSE
END SUB

FUNCTION ScoreColor (i)
    SELECT CASE i
        CASE 0: ScoreColor = 12
        CASE 1 TO 2: ScoreColor = 14
        CASE 3 TO 4: ScoreColor = 10
        CASE 5 TO 9: ScoreColor = 9
    END SELECT
END FUNCTION

FUNCTION ScoreString$ (h, i)
    s$ = STR$(i + 1)
    IF i = 9 THEN s$ = "10"
    s$ = s$ + "." + HiScores(h, i)
    ScoreString = s$
END FUNCTION

SUB ShowHiScores
    RedWindow 14
    LOCATE 6, 31
    COLOR 11
    PRINT "�롥�� ��ਠ�� ����"
    LOCATE 9, 38
    PRINT "������"
    l = 0
    s = 0
        DO
        h = l * 3 + s
        FOR i = 0 TO 1
            FOR j = 0 TO 2
                LOCATE 7 + i, 20 + j * 14
                COLOR 9
                IF i = l AND j = s THEN COLOR 13
                IF j = 0 THEN s$ = "  8 x 8"
                IF j = 1 THEN s$ = "16 x 16"
                IF j = 2 THEN s$ = "32 x 32"
                IF i = 0 THEN s$ = s$ + " ���."
                IF i = 1 THEN s$ = s$ + " ����."
                PRINT s$
            NEXT
        NEXT
        FOR i = 0 TO 9
            LOCATE 10 + i, 20
            COLOR ScoreColor(i)
            PRINT ScoreString(h, i)
        NEXT
        DO: LOOP UNTIL INKEY$ = ""
        DO
            k$ = INKEY$
        LOOP WHILE k$ = ""
        IF k$ = klf$ THEN s = s - 1
        IF k$ = krt$ THEN s = s + 1
        IF k$ = kup$ OR k$ = kdn$ THEN l = 1 - l
        IF s < 0 THEN s = 2
        IF s > 2 THEN s = 0
    LOOP UNTIL k$ = kes$ OR k$ = ken$

END SUB

'���⭮� ������ � �����⭮� �⪥
'x, y - �祩��
'n - ����� �ࠤ�樨
'i - �⥯��� �ࠤ�樨
SUB Square (x, y, n, i)
    PUT (220 + x * 20, 55 + y * 16), grc(0, n * 8 + i), PSET
END SUB

'��� ��������
'0 - 室 ᤥ���
'1 - ���
FUNCTION StepComputer (pl)

    CandidatesCalc pl

    '���� 㧭��� �� ���� �� �����-� 室 ���騬
    ac = FCount(1 - pl)
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            IF c(x, y) = ac THEN c(x, y) = 99
        NEXT
    NEXT


    '�ਮ���� � �� ����⥫�� ����樨
    IF level > 0 THEN
        b = size - 1
        IF c(0, 0) > 0 THEN c(0, 0) = 98
        IF c(0, b) > 0 THEN c(0, b) = 98
        IF c(b, 0) > 0 THEN c(b, 0) = 98
        IF c(b, b) > 0 THEN c(b, b) = 98
        FOR i = 2 TO b - 2
            IF c(i, 0) > 0 THEN c(i, 0) = 97
            IF c(i, b) > 0 THEN c(i, b) = 97
            IF c(0, i) > 0 THEN c(0, i) = 97
            IF c(b, i) > 0 THEN c(b, i) = 97

        NEXT
    END IF

    '��।��塞 ������訥 ����樨
    max = 0
    szs = size * size
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            cn = c(x, y)
            IF cn > max THEN
                max = cn
                REDIM cx(szs)
                REDIM cy(szs)
                n = 0
            END IF
            IF cn = max AND cn > 0 THEN
                cx(n) = x
                cy(n) = y
                n = n + 1
            END IF
        NEXT
    NEXT

    '�⫠��筮� - ��ᬮ�� ��� ��ਠ権
    'FOR x = 0 TO size - 1
    '    FOR y = 0 TO size - 1
    '        LOCATE 8 - y, (x + 1) * 3: PRINT c(x, y)
    '    NEXT
    'NEXT
    'SLEEP 0

    '������ 室, ��� ���㥬
    IF max > 0 THEN
        s = INT(RND * n)
        StepEnd cx(s), cy(s), pl
    ELSE
        StepComputer = 1
    END IF

END FUNCTION

'�����襭�� 室� - ���饭�� �襪 ��⨢����
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

'��� 祫�����
'0 - 室 ᤥ���
'1 - ���
'2 - ��室
FUNCTION StepHuman (gr(), pl)
    CandidatesCalc pl
    DO
        x = left + kx * wd
        y = bott + ky * hg

        IF c(kx, ky) > 0 THEN
            PUT (x, y), gr(0, 5), AND
            PUT (x, y), gr(0, 3 + pl), XOR
        ELSE
            PUT (x, y), gr(0, 8), AND
            PUT (x, y), gr(0, 6 + pl), XOR
        END IF
        DO
            k$ = INKEY$
        LOOP WHILE k$ = ""
    
        PUT (left + kx * wd, bott + ky * hg), gr(0, m(kx, ky) + 1), PSET
        IF k$ = klf$ AND kx > 0 THEN kx = kx - 1
        IF k$ = krt$ AND kx < size - 1 THEN kx = kx + 1
        IF k$ = kdn$ AND ky > 0 THEN ky = ky - 1
        IF k$ = kup$ AND ky < size - 1 THEN ky = ky + 1
        IF k$ = ken$ AND c(kx, ky) > 0 THEN
            StepEnd kx, ky, pl
            EXIT FUNCTION
        END IF
        IF k$ = " " THEN
            StepHuman = 1
            EXIT FUNCTION
        END IF
        IF k$ = kes$ THEN
            k = MiniMenu(0)
            IF k = 1 THEN SaveGame
            IF k > 0 THEN
                StepHuman = 2
                EXIT FUNCTION
            END IF
        END IF
    LOOP WHILE -1
END FUNCTION

'��।������ ������⥫�
'0,1 - ������
'2 - �த������ �����
'3 - �����
FUNCTION WhoWin (pass)
    p = 0
    p1 = 0
    p2 = 0
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            IF m(x, y) = -1 THEN p = 1
            IF m(x, y) = 0 THEN p0 = p0 + 1
            IF m(x, y) = 1 THEN p1 = p1 + 1
        NEXT
    NEXT
    IF p = 0 OR p0 = 0 OR p1 = 0 OR pass THEN
        WhoWin = 3
        IF p0 > p1 THEN WhoWin = 0
        IF p1 > p0 THEN WhoWin = 1
    ELSE
        WhoWin = 2
    END IF
END FUNCTION

SUB Win (w)

    RedWindow 5
    COLOR 11
    IF w = 3 THEN
        PUT (199, 158), grf(0, 0), PSET
        PUT (400, 158), grf(0, 1), PSET
        LOCATE 13, 38
        PRINT "�����!"
    ELSE
        PUT (300, 172), grf(0, w), PSET
        
        t$ = "������� " + pl(w) + "!"
        LOCATE 14, 41 - LEN(t$) / 2
        PRINT t$
    END IF
    PlaySound 2
    DO: LOOP UNTIL INKEY$ = ""
    DO: LOOP UNTIL INKEY$ <> ""

    '�᫨ �� ��� �뫠 ����㦥�� - 㤠�塞 䠩�
    IF IsLoadGame THEN KILL "game.sav"

    '�᫨ ��� �뫠 �� � �������஬ - � ⠡���� �� ��������
    IF pl(0) <> "Computer" AND pl(1) <> "Computer" THEN EXIT SUB

    '��।��塞 � ����� ⠡���� ४�म� �� ��������
    h = 0
    IF size = 16 THEN h = 1
    IF size = 32 THEN h = 2
    h = h + level * 3

    '��।��塞 ����
    sc = score(FirstPlayer)
    m = -1
    FOR i = 0 TO 9
        s$ = MID$(HiScores(h, i), 13, 5)
        s = VAL(s$)
        IF s$ = "....." THEN s = -9999
        IF sc > s THEN m = i: EXIT FOR
    NEXT
    IF m >= 0 THEN
        FOR i = 9 TO m + 1 STEP -1
            HiScores(h, i) = HiScores(h, i - 1)
        NEXT
        s$ = pl(FirstPlayer)
        DO
            s$ = s$ + "."
        LOOP UNTIL LEN(s$) = 12
        sc$ = STR$(sc)
        IF LEFT$(sc$, 1) = " " THEN sc$ = RIGHT$(sc$, LEN(sc$) - 1)
        s$ = s$ + sc$
        DO
            s$ = s$ + "."
        LOOP UNTIL LEN(s$) = 19
        s$ = s$ + DATE$ + ".." + TIME$

        HiScores(h, m) = s$
    END IF

    SaveParams

    RedWindow 11
    LOCATE 8, 37
    PRINT "������"
    FOR i = 0 TO 9
        LOCATE 9 + i, 20
        COLOR ScoreColor(i)
        PRINT ScoreString(h, i)
    NEXT
    a = 0
    DO
        IF m > -1 THEN
            LOCATE 9 + m, 20
            IF a = 0 THEN
                COLOR 15
            ELSE
                COLOR ScoreColor(m)
            END IF
            PRINT ScoreString(h, m)
            a = a + 1
            IF a > 1 THEN a = 0
            Delay .1
        END IF

    LOOP UNTIL INKEY$ <> ""

END SUB

