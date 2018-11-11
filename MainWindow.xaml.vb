Class MainWindow

    Dim turnDisp As TextBox = turnDisplay
    Dim selected As Piece
    Dim previous As Piece
    Dim fill As Button()

    Dim Signature = 1
    Dim Signature2 = 1
    Dim Signature3 = 1
    Dim Signature4 = 1
    Dim count = 0
    Dim value = 1

    Public Enum Turn
        White
        Black
    End Enum

    Public Enum Type
        Empty
        Pawn
        Rook
        Knight
        Bishop
        Queen
        King
    End Enum

    Private Structure Game
        Public pointsWhite As Integer
        Public pointsBlack As Integer
        Public checkWhite As Boolean
        Public checkBlack As Boolean
        Public turn As Turn
    End Structure

    Private Structure Piece
        Public Button As Button
        Public Team As Turn
        Public Column As Integer
        Public Row As Integer
        Public Selected As Boolean
        Public Threat As Boolean
        Public Type As Type
        Public hasMoved As Boolean
    End Structure

    Dim gameObject As Game
    Dim pieceSet(7, 7) As Piece
    Dim gameBoard(7, 7) As Piece

    Private Function getTrace()
        Dim checkPath As Button() = {x0, x1, x2, x3, x4, x5, x6, x7}
        Return checkPath
    End Function

    Private Function getAvailableMoves()
        Dim blankes As Button() = {blank0, blank1, blank2, blank3, blank4, blank5, blank6, blank7, blank8, blank9, blank10, blank11, blank12, blank13, blank14, blank15, blank16, blank17}
        Return blankes
    End Function

    '------------------------------------------------------------------------
    '                            Game Initializer
    '------------------------------------------------------------------------

    Public Sub Main()
        Application.Children.Remove(titleBG)
        Application.Children.Remove(titleButton)

        My.Computer.Audio.Play(My.Resources.sword_draw, AudioPlayMode.Background)

        gameObject.checkBlack = False
        gameObject.checkBlack = False

        Dim x, y
        Dim z = 0
        Dim wButtons As Button() = {wR, wKn, wB, wK, wQ, wB2, wKn2, wR2, wp1B, wp2B, wp3B, wp4B, wp5B, wp6B, wp7B, wp8B}
        Dim bButtons As Button() = {bR, bKn, bB, bQ, bK, bB2, bKn2, bR2, bp1B, bp2B, bp3B, bp4B, bp5B, bp6B, bp7B, bp8B}
        Dim empties As Button() = {empty0, empty1, empty2, empty3, empty4, empty5, empty6, empty7, empty8, empty9, empty10, empty11, empty12, empty13, empty14, empty15, empty16, empty17, empty18, empty19, empty20, empty21, empty22, empty23, empty24, empty25, empty26, empty27, empty28, empty29, empty30, empty31}

        For x = 2 To 5
            For y = 0 To 7
                pieceSet(x, y).Button = empties(z)
                pieceSet(x, y).Button.Visibility = Visibility.Hidden
                pieceSet(x, y).Type = Type.Empty
                z = z + 1
            Next
        Next
        For x = 0 To 7
            For y = 0 To 7
                pieceSet(x, y).Threat = False
                pieceSet(x, y).Selected = False
                pieceSet(x, y).Row = x
                pieceSet(x, y).Column = y

                pieceSet(0, 0).Type = Type.Rook
                pieceSet(0, 1).Type = Type.Knight
                pieceSet(0, 2).Type = Type.Bishop
                pieceSet(0, 3).Type = Type.Queen
                pieceSet(0, 4).Type = Type.King
                pieceSet(0, 5).Type = Type.Bishop
                pieceSet(0, 6).Type = Type.Knight
                pieceSet(0, 7).Type = Type.Rook

                pieceSet(0, y).Button = bButtons(y)
                pieceSet(0, y).Team = Turn.Black
                pieceSet(1, y).Button = bButtons(y + 8)
                pieceSet(1, y).Team = Turn.Black
                pieceSet(1, y).Type = Type.Pawn

                pieceSet(7, 0).Type = Type.Rook
                pieceSet(7, 1).Type = Type.Knight
                pieceSet(7, 2).Type = Type.Bishop
                pieceSet(7, 3).Type = Type.King
                pieceSet(7, 4).Type = Type.Queen
                pieceSet(7, 5).Type = Type.Bishop
                pieceSet(7, 6).Type = Type.Knight
                pieceSet(7, 7).Type = Type.Rook

                pieceSet(7, y).Button = wButtons(y)
                pieceSet(7, y).Team = Turn.White
                pieceSet(6, y).Button = wButtons(y + 8)
                pieceSet(6, y).Team = Turn.White
                pieceSet(6, y).Type = Type.Pawn

                gameBoard(x, y) = pieceSet(x, y)
            Next
        Next
        For x = 0 To 1
            For y = 0 To 7
                gameBoard(x, y).Button.IsEnabled = False
            Next
        Next
    End Sub

    Public Sub NewGame()
        gameObject.turn = Turn.Black
        UpdateGame()
        For x = 0 To 7
            For y = 0 To 7
                gameBoard(x, y) = pieceSet(x, y)
                If (pieceSet(x, y).Type <> Type.Empty) Then
                    gameBoard(x, y).Button.Visibility = Visibility.Visible
                    Grid.SetColumn(pieceSet(x, y).Button, y)
                    Grid.SetRow(pieceSet(x, y).Button, x)
                End If
            Next
        Next
        gameObject.checkBlack = False
        gameObject.checkWhite = False
        gameOver.Visibility = Visibility.Collapsed
    End Sub


    '------------------------------------------------------------------------
    '                           Game Functions
    '------------------------------------------------------------------------


    Public Sub UpdateGame()
        If gameObject.turn = Turn.White Then
            turnDisplay.Text = "Black"
            gameObject.turn = Turn.Black
            For x = 0 To 7
                For y = 0 To 7
                    If (gameBoard(x, y).Team = Turn.Black) Then
                        gameBoard(x, y).Button.IsEnabled = True
                    Else
                        gameBoard(x, y).Button.IsEnabled = False
                    End If
                Next
            Next
        ElseIf gameObject.turn = Turn.Black Then
            turnDisplay.Text = "White"
            gameObject.turn = Turn.White
            For x = 0 To 7
                For y = 0 To 7
                    If (gameBoard(x, y).Team = Turn.White) Then
                        gameBoard(x, y).Button.IsEnabled = True
                    Else
                        gameBoard(x, y).Button.IsEnabled = False
                        gameBoard(x, y).Button.OverridesDefaultStyle = True

                    End If
                Next
            Next
        End If
        CheckKing()
        If gameObject.checkBlack = True Then
            blackCheck.IsChecked = True
        Else
            blackCheck.IsChecked = False
        End If
        If gameObject.checkWhite = True Then
            whiteCheck.IsChecked = True
        Else
            whiteCheck.IsChecked = False
        End If
    End Sub

    Public Sub TracePath(start As Button, finish As Button, signature As Integer)
        Dim checkPath = getTrace()
        Dim startRow, startCol, finishRow, finishCol, counter
        counter = 0
        startRow = Grid.GetRow(start)
        startCol = Grid.GetColumn(start)
        finishRow = Grid.GetRow(finish)
        finishCol = Grid.GetColumn(finish)

        If Grid.GetRow(start) = Grid.GetRow(finish) Then
            While startCol <> finishCol - signature
                Grid.SetRow(checkPath(counter), startRow)
                Grid.SetColumn(checkPath(counter), startCol)
                startCol += signature
                counter += 1
            End While
        ElseIf Grid.GetColumn(start) = Grid.GetColumn(finish) Then
            While startRow <> finishRow - signature
                Grid.SetColumn(checkPath(counter), startCol)
                Grid.SetRow(checkPath(counter), startRow)
                startRow += signature
                counter += 1
            End While
        Else
            While startCol <> finishCol - signature And startRow <> finishRow - signature
                Grid.SetColumn(checkPath(counter), startCol)
                Grid.SetRow(checkPath(counter), startRow)
                startCol += signature
                startRow += signature
                counter += 1
            End While
        End If
    End Sub

    Public Sub CompareTrace(ByRef blank As Button)
        Dim checkPath = getTrace()
        Dim x, y, z, a, b
        x = 0
        While x < checkPath.Length
            y = Grid.GetRow(checkPath(x))
            z = Grid.GetColumn(checkPath(x))
            a = Grid.GetRow(blank)
            b = Grid.GetColumn(blank)

            If y <> a And z <> b Then
                blank.Visibility = Visibility.Collapsed
            End If
            x += 1
        End While
    End Sub

    Public Sub CheckKing()
        Signature = 1
        Dim l, z
        Dim king As Piece = Nothing

        Dim refRow = 0
        Dim refCol = 0

        For l = 0 To 1
            Signature2 = 1
            Signature3 = 1
            z = 0
            For x = 0 To 7
                For y = 0 To 7
                    If gameBoard(x, y).Type = Type.King Then
                        If gameBoard(x, y).Team = Turn.White And Signature = -1 Then
                            king = gameBoard(x, y)
                            refRow = x
                            refCol = y
                        ElseIf gameBoard(x, y).Team = Turn.Black And Signature = 1 Then
                            king = gameBoard(x, y)
                            refRow = x
                            refCol = y
                        End If
                    End If
                Next
            Next
            '-------------- Diagonals ----------------
            For x = 0 To 1
                For y = 0 To 1
                    While (refRow + (value * Signature * Signature2) < 8) And (refRow + (value * Signature * Signature2) > -1) And (refCol + (value * Signature * Signature3) < 8) And (refCol + (value * Signature * Signature3) > -1)
                        If (gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type = Type.Empty Or gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Team <> king.Team) Then


                            If gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type <> Type.Empty And gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Team <> king.Team Then
                                If gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type = Type.Bishop Or gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type = Type.Queen Then
                                    'TracePath(king.Button, gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Button, Signature)
                                    z = 1
                                End If
                                If value = 1 And gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type = Type.Pawn Then
                                    'TracePath(king.Button, gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Button, Signature)
                                    z = 1
                                End If
                                Exit While
                            End If
                            value += 1
                        Else
                            Exit While
                        End If
                    End While
                    value = 1
                    If (Signature2 = 1) Then
                        Signature2 = -1
                    Else
                        Signature2 = 1
                    End If
                Next
                Signature3 = -1
            Next

            '------------- Perpendiculars --------------
            Signature4 = 0
            For x = 0 To 1
                For y = 0 To 1
                    While ((refRow + (value * Signature * Signature2 * Signature3) < 8) And (refRow + (value * Signature * Signature2 * Signature3) > -1) And (refCol + (value * Signature * Signature2 * Signature4) < 8) And (refCol + (value * Signature * Signature2 * Signature4) > -1))
                        If (gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Type = Type.Empty Or gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Team <> king.Team) Then

                            If gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Type <> Type.Empty And gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Team <> king.Team Then
                                If gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Type = Type.Rook Or gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Type = Type.Queen Then
                                    'TracePath(king.Button, gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Button, Signature)
                                    z = 1
                                End If
                                Exit While
                            End If
                            value += 1
                        Else
                            Exit While
                        End If
                    End While
                    count += 1
                    value = 1
                    If Signature2 = 1 Then
                        Signature2 = -1
                    Else
                        Signature2 = 1
                    End If
                Next
                value = 1
                Signature4 = 1
                Signature3 = 0
            Next

            If z = 1 Then
                CheckSignatures(Signature, 1)
            Else
                CheckSignatures(Signature, 0)
            End If
            Signature = -1
        Next
    End Sub

    Public Sub CheckPrevention(ByRef blank As Button)
        Signature = 1
        Dim l, z

        Dim refRow = Grid.GetRow(blank)
        Dim refCol = Grid.GetColumn(blank)

        For l = 0 To 1
            Signature2 = 1
            Signature3 = 1
            z = 0
            '-------------- Diagonals ----------------
            For x = 0 To 1
                For y = 0 To 1
                    While (refRow + (value * Signature * Signature2) < 8) And (refRow + (value * Signature * Signature2) > -1) And (refCol + (value * Signature * Signature3) < 8) And (refCol + (value * Signature * Signature3) > -1)
                        If (gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type = Type.Empty Or gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Team <> gameObject.turn) Then
                            If gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type <> Type.Empty And gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Team <> gameObject.turn Then
                                If gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type = Type.Bishop Or gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type = Type.Queen Then
                                    z = 1
                                End If
                                If value = 1 And gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type = Type.Pawn Then
                                    z = 1
                                End If
                                Exit While
                            End If
                            value += 1
                        Else
                            Exit While
                        End If
                    End While
                    value = 1
                    If (Signature2 = 1) Then
                        Signature2 = -1
                    Else
                        Signature2 = 1
                    End If
                Next
                Signature3 = -1
            Next

            '------------- Perpendiculars --------------
            Signature4 = 0
            For x = 0 To 1
                For y = 0 To 1
                    While ((refRow + (value * Signature * Signature2 * Signature3) < 8) And (refRow + (value * Signature * Signature2 * Signature3) > -1) And (refCol + (value * Signature * Signature2 * Signature4) < 8) And (refCol + (value * Signature * Signature2 * Signature4) > -1))
                        If (gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Type = Type.Empty Or gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Team <> gameObject.turn) Then

                            If gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Type <> Type.Empty And gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Team <> gameObject.turn Then
                                If gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Type = Type.Rook Or gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Type = Type.Queen Then
                                    z = 1
                                End If
                                Exit While
                            End If
                            value += 1
                        Else
                            Exit While
                        End If
                    End While
                    value = 1
                    If Signature2 = 1 Then
                        Signature2 = -1
                    Else
                        Signature2 = 1
                    End If
                Next
                value = 1
                Signature4 = 1
                Signature3 = 0
            Next
            If z = 1 Then
                blank.Visibility = Visibility.Collapsed
            End If
            Signature = -1
        Next
    End Sub

    Public Sub CheckSignatures(Signature As Integer, Signature2 As Integer)
        If Signature2 = 1 Then
            If Signature = 1 Then
                check(Turn.Black, True)
            Else
                check(Turn.White, True)
            End If
        Else
            If Signature = 1 Then
                check(Turn.Black, False)
            Else
                check(Turn.White, False)
            End If
        End If
    End Sub

    Public Sub check(identifier As Turn, isChecked As Boolean)
        If identifier = Turn.White Then
            If isChecked = True Then
                gameObject.checkBlack = True
                My.Computer.Audio.Play(My.Resources.ohno, AudioPlayMode.Background)
            Else
                gameObject.checkBlack = False
            End If
        Else
            If isChecked = True Then
                gameObject.checkWhite = True
                My.Computer.Audio.Play(My.Resources.huh, AudioPlayMode.Background)
            Else
                gameObject.checkWhite = False
            End If
        End If
    End Sub

    Public Sub playsound(type As String)
        If gameObject.turn = Turn.Black Then
            If type = "hit" Then
                My.Computer.Audio.Play(My.Resources.hit2, AudioPlayMode.Background)
            Else
                My.Computer.Audio.Play(My.Resources._240788__f4ngy__knife_hitting_wood, AudioPlayMode.Background)
            End If

        Else
            If type = "hit" Then
                My.Computer.Audio.Play(My.Resources.hit, AudioPlayMode.Background)
            Else
                My.Computer.Audio.Play(My.Resources.clack, AudioPlayMode.Background)
            End If
        End If
    End Sub
    '------------------------------------------------------------------------
    '                              Piece Logic
    '------------------------------------------------------------------------

    '---------Select Function----------
    Public Sub MvPiece(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs)
        Dim blankes As Button() = getAvailableMoves()
        Dim nextRow, nextCol, refRow, refCol
        Signature = 1
        Signature2 = 1
        Signature3 = 1
        Signature4 = 1
        count = 0
        value = 1

        For x = 0 To blankes.Length - 1
            If blankes(x) IsNot Nothing Then
                blankes(x).Visibility = Visibility.Hidden
            End If
        Next

        'Iterate Through Piece Array To Find Matching Button From Sender
        For x = 0 To 7
            For y = 0 To 7
                If ReferenceEquals(sender, gameBoard(x, y).Button) Then
                    previous = selected
                    selected = gameBoard(x, y)
                    refRow = Grid.GetRow(sender)
                    refCol = Grid.GetColumn(sender)
                End If
            Next
        Next

        If (selected.Team = Turn.White) Then
            Signature = -1
        End If
        '---------------------------------------------KING-----------------------------------------
        If (selected.Type = Type.King) Then
            For x = 0 To 1
                For y = 0 To 1
                    If (refRow + (value * Signature * Signature2) < 8) And (refRow + (value * Signature * Signature2) > -1) And (refCol + (value * Signature * Signature3) < 8) And (refCol + (value * Signature * Signature3) > -1) Then

                        If (gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type = Type.Empty Or gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Team <> selected.Team) Then
                            blankes(count).Visibility = Visibility.Visible
                            Grid.SetRow(blankes(count), refRow + (value * Signature * Signature2))
                            Grid.SetColumn(blankes(count), refCol + (value * Signature * Signature3))
                            CheckPrevention(blankes(count))
                            count += 1
                            value += 1
                        End If
                    End If
                    value = 1
                    If (Signature2 = 1) Then
                        Signature2 = -1
                    Else
                        Signature2 = 1
                    End If
                Next
                Signature3 = -1
            Next

            Signature4 = 0
            For x = 0 To 1
                For y = 0 To 1
                    If ((refRow + (value * Signature * Signature2 * Signature3) < 8) And (refRow + (value * Signature * Signature2 * Signature3) > -1) And (refCol + (value * Signature * Signature2 * Signature4) < 8) And (refCol + (value * Signature * Signature2 * Signature4) > -1)) Then
                        If (gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Type = Type.Empty Or gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Team <> selected.Team) Then
                            blankes(count).Visibility = Visibility.Visible
                            Grid.SetRow(blankes(count), refRow + (value * Signature * Signature2 * Signature3))
                            Grid.SetColumn(blankes(count), refCol + (value * Signature * Signature2 * Signature4))
                            CheckPrevention(blankes(count))
                            count += 1
                            value += 1
                        End If
                    End If
                    value = 1
                    If Signature2 = 1 Then
                        Signature2 = -1
                    Else
                        Signature2 = 1
                    End If
                Next
                value = 1
                Signature4 = 1
                Signature3 = 0
            Next

        End If
        '--------------------------------------------QUEEN-----------------------------------------
        If (selected.Type = Type.Queen) Then
            For x = 0 To 1
                For y = 0 To 1
                    While (refRow + (value * Signature * Signature2) < 8) And (refRow + (value * Signature * Signature2) > -1) And (refCol + (value * Signature * Signature3) < 8) And (refCol + (value * Signature * Signature3) > -1)
                        If (gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type = Type.Empty Or gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Team <> selected.Team) Then
                            blankes(count).Visibility = Visibility.Visible
                            Grid.SetRow(blankes(count), refRow + (value * Signature * Signature2))
                            Grid.SetColumn(blankes(count), refCol + (value * Signature * Signature3))
                            count += 1
                            If gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type <> Type.Empty And gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Team <> selected.Team Then
                                Exit While
                            End If
                            value += 1
                        Else
                            Exit While
                        End If
                    End While
                    value = 1
                    If (Signature2 = 1) Then
                        Signature2 = -1
                    Else
                        Signature2 = 1
                    End If
                Next
                Signature3 = -1
            Next

            Signature4 = 0
            For x = 0 To 1
                For y = 0 To 1
                    While ((refRow + (value * Signature * Signature2 * Signature3) < 8) And (refRow + (value * Signature * Signature2 * Signature3) > -1) And (refCol + (value * Signature * Signature2 * Signature4) < 8) And (refCol + (value * Signature * Signature2 * Signature4) > -1))
                        If (gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Type = Type.Empty Or gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Team <> selected.Team) Then
                            blankes(count).Visibility = Visibility.Visible
                            Grid.SetRow(blankes(count), refRow + (value * Signature * Signature2 * Signature3))
                            Grid.SetColumn(blankes(count), refCol + (value * Signature * Signature2 * Signature4))
                            count += 1
                            If gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Type <> Type.Empty And gameBoard(refRow + (value * Signature * Signature2 * Signature3), refCol + (value * Signature * Signature2 * Signature4)).Team <> selected.Team Then
                                Exit While
                            End If
                            value += 1
                        Else
                            Exit While
                        End If
                    End While
                    value = 1
                    If Signature2 = 1 Then
                        Signature2 = -1
                    Else
                        Signature2 = 1
                    End If
                Next
                value = 1
                Signature4 = 1
                Signature3 = 0
            Next
        End If
        '--------------------------------------------BISHOP----------------------------------------
        If (selected.Type = Type.Bishop) Then
            For x = 0 To 1
                For y = 0 To 1
                    While (refRow + (value * Signature * Signature2) < 8) And (refRow + (value * Signature * Signature2) > -1) And (refCol + (value * Signature * Signature3) < 8) And (refCol + (value * Signature * Signature3) > -1)
                        If (gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type = Type.Empty Or gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Team <> selected.Team) Then
                            blankes(count).Visibility = Visibility.Visible
                            Grid.SetRow(blankes(count), refRow + (value * Signature * Signature2))
                            Grid.SetColumn(blankes(count), refCol + (value * Signature * Signature3))
                            count += 1
                            If gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type <> Type.Empty And gameBoard(refRow + (value * Signature * Signature2), refCol + (value * Signature * Signature3)).Team <> selected.Team Then
                                Exit While
                            End If
                            value += 1
                        Else
                            Exit While
                        End If
                    End While
                    value = 1
                    If (Signature2 = 1) Then
                        Signature2 = -1
                    Else
                        Signature2 = 1
                    End If
                Next

                Signature3 = -1
            Next
        End If
        '--------------------------------------------KNIGHT----------------------------------------
        If (selected.Type = Type.Knight) Then
            For x = 0 To 1
                For y = 0 To 1
                    If ((refRow + (2 * value * Signature * Signature2) < 8) And (refRow + (2 * value * Signature * Signature2) > -1) And (refCol + (value * Signature * Signature3) < 8) And (refCol + (value * Signature * Signature3) > -1)) Then
                        If (gameBoard(refRow + (2 * value * Signature * Signature2), refCol + (value * Signature * Signature3)).Type = Type.Empty Or gameBoard(refRow + (2 * value * Signature * Signature2), refCol + (value * Signature * Signature3)).Team <> selected.Team) Then
                            blankes(count).Visibility = Visibility.Visible
                            Grid.SetRow(blankes(count), refRow + (2 * value * Signature * Signature2))
                            Grid.SetColumn(blankes(count), refCol + (value * Signature * Signature3))
                            count += 1
                        End If
                    End If
                    If (Signature3 = 1) Then
                        Signature3 = -1
                    Else
                        Signature3 = 1
                    End If
                Next
                Signature2 = -1
            Next

            For x = 0 To 1
                For y = 0 To 1
                    If ((refRow + (value * Signature * Signature2) < 8) And (refRow + (value * Signature * Signature2) > -1) And (refCol + (2 * value * Signature * Signature3) < 8) And (refCol + (2 * value * Signature * Signature3) > -1)) Then
                        If (gameBoard(refRow + (value * Signature * Signature2), refCol + (2 * value * Signature * Signature3)).Type = Type.Empty Or gameBoard(refRow + (value * Signature * Signature2), refCol + (2 * value * Signature * Signature3)).Team <> selected.Team) Then
                            blankes(count).Visibility = Visibility.Visible
                            Grid.SetRow(blankes(count), refRow + (value * Signature * Signature2))
                            Grid.SetColumn(blankes(count), refCol + (2 * value * Signature * Signature3))
                            count += 1
                        End If
                    End If
                    If (Signature2 = 1) Then
                        Signature2 = -1
                    Else
                        Signature2 = 1
                    End If
                Next
                Signature3 = -1
            Next
        End If

        '---------------------------------------------ROOK-----------------------------------------
        If (selected.Type = Type.Rook) Then
            Signature4 = 0
            count = 0

            'Signature 1 Identifies Team
            'Signature 2 Identifies Polarity Of Row/Column Traversal
            'Signature 3 Identifies If Row/Column Should Change

            For x = 0 To 1
                'Switch From Row to Column
                nextRow = refRow + (value * Signature * Signature2 * Signature3)
                nextCol = refCol + (value * Signature * Signature2 * Signature4)
                For y = 0 To 1
                    'Change Polarity
                    nextRow = refRow + (value * Signature * Signature2 * Signature3)
                    nextCol = refCol + (value * Signature * Signature2 * Signature4)
                    'Loop Until The Next Row Or Column Hits An Array Boundary
                    While ((nextRow < 8) And (nextRow > -1) And (nextCol < 8) And (nextCol > -1))
                        If (gameBoard(nextRow, nextCol).Type = Type.Empty Or gameBoard(nextRow, nextCol).Team <> selected.Team) Then
                            'Set Movement Spaces
                            blankes(count).Visibility = Visibility.Visible
                            Grid.SetRow(blankes(count), nextRow)
                            Grid.SetColumn(blankes(count), nextCol)
                            'If gameObject.checkWhite Or gameObject.checkBlack Then
                            'If Signature = 1 Then
                            'CompareTrace(blankes(count))
                            'Else
                            '   CompareTrace(blankes(count))
                            'End If
                            'End If
                            count += 1
                            value += 1
                        Else
                            Exit While
                        End If
                        'If The Next Row/Column Is An Enemy Piece, End The Current Loop
                        If gameBoard(nextRow, nextCol).Type <> Type.Empty And gameBoard(nextRow, nextCol).Team <> selected.Team Then
                            Exit While
                        End If
                        'Increment Row Or Column
                        nextRow = refRow + (value * Signature * Signature2 * Signature3)
                        nextCol = refCol + (value * Signature * Signature2 * Signature4)
                    End While
                    value = 1
                    If Signature2 = 1 Then
                        Signature2 = -1
                    Else
                        Signature2 = 1
                    End If
                Next
                value = 1
                Signature4 = 1
                Signature3 = 0
            Next
        End If
        '---------------------------------------------PAWN-----------------------------------------
        If (selected.Type = Type.Pawn) Then
            If gameBoard(refRow + Signature, refCol).Type = Type.Empty Then
                blankes(count).Visibility = Visibility.Visible
                Grid.SetRow(blankes(count), refRow + Signature)
                Grid.SetColumn(blankes(count), refCol)
                count += 1
            End If
            If (selected.hasMoved = False) Then
                If gameBoard(refRow + (2 * Signature), refCol).Type = Type.Empty Then
                    blankes(count).Visibility = Visibility.Visible
                    Grid.SetRow(blankes(count), refRow + (2 * Signature))
                    Grid.SetColumn(blankes(count), refCol)
                    count += 1
                End If
            End If
            If (refRow + Signature <> 8 And refRow + Signature <> -1) Then
                If (refCol + Signature <> 8 And refCol + Signature <> -1) Then
                    If gameBoard(refRow + Signature, refCol + Signature).Type <> Type.Empty And gameBoard(refRow + Signature, refCol + Signature).Team <> selected.Team Then
                        blankes(count).Visibility = Visibility.Visible
                        Grid.SetRow(blankes(count), refRow + Signature)
                        Grid.SetColumn(blankes(count), refCol + Signature)
                        count += 1
                    End If
                End If
                If (refCol - Signature <> -1 And refCol - Signature <> 8) Then
                    If gameBoard(refRow + Signature, refCol - Signature).Type <> Type.Empty And gameBoard(refRow + Signature, refCol - Signature).Team <> selected.Team Then
                        blankes(count).Visibility = Visibility.Visible
                        Grid.SetRow(blankes(count), refRow + Signature)
                        Grid.SetColumn(blankes(count), refCol - Signature)
                        count += 1
                    End If
                End If
            End If
        End If

    End Sub

    Public Sub Move(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs)
        Dim blankes As Button() = getAvailableMoves()
        For x = 0 To blankes.Length - 1
            blankes(x).Visibility = Visibility.Hidden
        Next

        Dim moveTo As Button = sender
        Dim row = Grid.GetRow(sender)
        Dim col = Grid.GetColumn(sender)
        Dim temp As Piece
        Grid.SetRow(selected.Button, row)
        Grid.SetColumn(selected.Button, col)
        If gameBoard(row, col).Type <> Type.Empty Then
            playsound("hit")
            'If King Is Captured, End Game
            If (gameBoard(row, col).Type = Type.King) Then
                My.Computer.Audio.Play(My.Resources.gameOver, AudioPlayMode.WaitToComplete)
                gameOver.Visibility = Visibility.Visible
                Exit Sub
            End If
            gameBoard(row, col).Type = Type.Empty
            gameBoard(row, col).Button.Visibility = Visibility.Hidden
        Else
            playsound("move")
        End If

        'Exchange Spots Between The Selected Piece And Chosen Destination
        For x = 0 To 7
            For y = 0 To 7
                If selected.Button Is gameBoard(x, y).Button Then
                    temp = gameBoard(x, y)
                    gameBoard(x, y) = gameBoard(row, col)
                    gameBoard(row, col) = temp
                    gameBoard(row, col).hasMoved = True
                End If
            Next
        Next

        UpdateGame()
    End Sub
End Class
