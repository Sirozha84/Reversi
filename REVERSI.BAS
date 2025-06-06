DECLARE FUNCTION WhoWin! (pass!)
DECLARE SUB CandidatesCalc (pl!)
DECLARE SUB Win (w!)
DECLARE FUNCTION FCount! (pl!)
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

DIM SHARED m(31, 31)    '��஢�� ����
DIM SHARED level        '�஢��� ᫮�����
DIM SHARED size         '������
DIM SHARED pl(2) AS STRING   '����� ��ப��
DIM SHARED left
DIM SHARED bott
DIM SHARED wd
DIM SHARED hg
DIM SHARED kx           '�����
DIM SHARED ky
DIM SHARED c(31, 31)    '����� ��� ���᫥��� ������⮢

Init
level = 1
size = 8
NewGame

'���᫥��� ������ �������⮢
'0 - �᫨ 室� ���, 1 � ����� - ������⢮ "�������" ��⨢�����
SUB CandidatesCalc (pl)
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
        NEXT
    NEXT
END SUB

'������ ������⢠ "�������" � ���⪥ cx, cy
'mx, my - ���ࠢ�����
'pl - ����
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
SUB Game (gr(), move)

    '��।��塞 ࠧ���� ����� � �襪
    SELECT CASE size
        CASE 8:
            left = 110
            bott = 1
            wd = 54
            hg = 41
        CASE 16:
            left = 100
            bott = 3
            wd = 28
            hg = 20
        CASE 32:
            left = 98 '114
            bott = 2
            wd = 14
            hg = 10
    END SELECT

    '���㥬 ������
    FOR i = 0 TO 1
        PUT (i * 546, 0), grt(0, 0), PSET
        PUT (27 + i * 546, 263), grf(0, i), PSET
    NEXT
    
    COLOR 14
    FOR i = 0 TO 1
        LOCATE 10, 7 + i * 68 - LEN(pl(i + 1)) / 2
        PRINT pl(i + 1)
    NEXT

    '���㥬 ����
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            PUT (left + x * wd, bott + y * hg), gr(0, m(x, y)), PSET
        NEXT
    NEXT

    '��஢�� 横�
    DIM nm(size - 1, size - 1)
    pass = 0
    endgame = 0
    DO

        '������塞 ⠡��
        k = 94 / size / size
        COLOR 11
        FOR i = 0 TO 1
            p = 2 - i
            s = FCount(1 + i)
            a = FCount(2 - i)
            LOCATE 14, 4 + i * 68
            PRINT s
            LOCATE 18, 4 + i * 68
            PRINT s - a
            LINE (i * 546, 28)-(93 + i * 546, 41), 15, B
            LINE (i * 546, 28)-(s * k + i * 546, 41), 11 - (1 + i), BF
            LINE (s * k + i * 546, 28)-((s + a) * k + i * 546, 41), 11 - (2 - i), BF
            
        NEXT
        COLOR 11 - move
        LOCATE 1, 1
        PRINT TAB(INT(41 - (LEN(pl(move)) + 7) / 2));
        PRINT "�����: " + pl(move); TAB(80);
        LOCATE 2, 29
        COLOR 13

        '��।��塞, �� ᥩ�� 室��
        IF pl(move) <> "Computer" THEN
            '��� ������ 祫����
            PRINT "�஡�� - ���, Esc - ����"
            s = StepHuman(gr(), move)
        ELSE
            '��� ������ ��������
            PRINT " �������, ��������..."
            s = StepComputer(move)
        END IF
        IF s = 1 THEN pass = pass + 1 ELSE pass = 0 '��� ����
        IF s = 2 THEN endgame = -1                  '����� ��室�

        '������塞 ����
        FOR x = 0 TO size - 1
            FOR y = 0 TO size - 1
                IF first OR nm(x, y) <> m(x, y) THEN
                    nm(x, y) = m(x, y)
                    PUT (left + x * wd, bott + y * hg), gr(0, m(x, y)), PSET
                END IF
            NEXT
        NEXT
        
        '��।��塞, ���� �� ������
        w = WhoWin(pass > 1)
        IF w > 0 THEN
            Win (w)
            endgame = -1
        END IF

        '���室 室�
        move = 3 - move

    LOOP WHILE NOT endgame
    IF w > 0 THEN LOCATE 15, 30: PRINT "��� ����砭�"

END SUB

'���樠������, ����㧪� 䠩��� ��䨪�
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

'����㧪� �ࠩ⮢
'm() - ���ᨢ, ᮤ�ঠ騩 �ࠩ�
'file - ��� 䠩��
'cn - �������⢮ �ࠩ⮢
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

'����� ����� ����
SUB NewGame

    '�����⮢�� ����
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            m(x, y) = 0
        NEXT
    NEXT
    m(size / 2, size / 2) = 1
    m(size / 2 - 1, size / 2 - 1) = 1
    m(size / 2, size / 2 - 1) = 2
    m(size / 2 - 1, size / 2) = 2

    '������⮢�� ��ப��
    pl(1) = "��ࣥ�"
    pl(2) = "Computer+"
    move = 1
    kx = size / 2 - 2
    ky = size / 2

    '����� �㦭��� ����
    SELECT CASE size
        CASE 8:
            Game grf8(), move
        CASE 16:
            Game grf16(), move
        CASE 32:
            Game grf32(), move
    END SELECT

END SUB

'��� ��������
'0 - 室 ᤥ���
'1 - ���
FUNCTION StepComputer (pl)

    CandidatesCalc pl

    '���� 㧭��� �� ���� �� �����-� 室 ���騬
    ac = FCount(3 - pl)
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
        IF k$ = " " THEN
            StepHuman = 1
            EXIT FUNCTION
        END IF
        IF k$ = kes$ THEN
            PCOPY 0, 1
            LOCATE 15, 30: PRINT "����"
            LOCATE 16, 30: PRINT "Esc - �த������"
            LOCATE 17, 30: PRINT "Enter - ��室"
            DO
                k$ = INKEY$
            LOOP WHILE k$ = ""
            IF k$ = ken$ THEN
                StepHuman = 2
                EXIT FUNCTION
            END IF
            PCOPY 1, 0
        END IF
    LOOP WHILE 1 = 1
END FUNCTION

'��।������ ������⥫�
'0 - �த������ �����                
'1,2 - ������
'3 - �����
FUNCTION WhoWin (pass)
    p = 0
    p1 = 0
    p2 = 0
    FOR x = 0 TO size - 1
        FOR y = 0 TO size - 1
            IF m(x, y) = 0 THEN p = 1
            IF m(x, y) = 1 THEN p1 = p1 + 1
            IF m(x, y) = 2 THEN p2 = p2 + 1
        NEXT
    NEXT
    IF p = 0 OR p1 = 0 OR p2 = 0 OR pass THEN
        WhoWin = 3
        IF p1 > p2 THEN WhoWin = 1
        IF p2 > p1 THEN WhoWin = 2
    ELSE
        WhoWin = 0
    END IF
END FUNCTION

SUB Win (w)
    LOCATE 13, 30
    IF w = 3 THEN
        PRINT "�����!"
    ELSE
        PRINT "�������: "; pl(w)
    END IF
END SUB

