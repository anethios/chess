Class MainWindow

    Dim turnSequence As Integer
    Dim turnDisp As TextBox = turnDisplay
    Dim gameStarted As Integer = 0
    Dim selected As Piece


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
        Public turn As Turn
    End Structure

    Private Structure Piece
        Public Button As Button
        Public Team As Turn
        Public Column As Integer
        Public Row As Integer
        Public Selected As Boolean
        Public Captured As Boolean
        Public Type As Type
        Public hasMoved As Boolean
    End Structure

    Dim gameObject As Game
    Dim pieceSet(7, 7) As Piece
    Dim gameBoard(7, 7) As Piece

    Private Function getAvailableMoves()
        Dim blankes As Button() = {blank0, blank1, blank2, blank3, blank4, blank5, blank6, blank7, blank8, blank9, blank10, blank11, blank12, blank13, blank14, blank15, blank16, blank17}
        Return blankes
    End Function

    Public Sub Main()
        Application.Children.Remove(titleBG)
        Application.Children.Remove(titleButton)

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
                pieceSet(x, y).Captured = False
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

    '------------------------------------------------------------------------
    '                           Game Functions
    '------------------------------------------------------------------------
    Public Sub UpdateGame()
        If gameObject.turn = Turn.White Then
            turnDisplay.Text = "Turn: Black"
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
            turnDisplay.Text = "Turn: White"
            gameObject.turn = Turn.White
            For x = 0 To 7
                For y = 0 To 7
                    If (gameBoard(x, y).Team = Turn.White) Then
                        gameBoard(x, y).Button.IsEnabled = True
                    Else
                        gameBoard(x, y).Button.IsEnabled = False
                    End If
                Next
            Next
        End If
    End Sub

    Public Function GetButtonsW()
        Dim wButtons As Button() = {wR, wKn, wB, wK, wQ, wB2, wKn2, wR2, wp1B, wp2B, wp3B, wp4B, wp5B, wp6B, wp7B, wp8B}
        Return wButtons
    End Function

    Public Function GetButtonsB()
        Dim bButtons As Button() = {bR, bKn, bB, bQ, bK, bB2, bKn2, bR2, bp1B, bp2B, bp3B, bp4B, bp5B, bp6B, bp7B, bp8B}
        Return bButtons
    End Function

    '------------------------------------------------------------------------
    '                            Game Initializer
    '------------------------------------------------------------------------

    Public Sub NewGame()
        gameObject.turn = Turn.White

        For x = 0 To 7
            For y = 0 To 7
                gameBoard(x, y) = pieceSet(x, y)
                gameBoard(x, y).Button.Visibility = True
                If (pieceSet(x, y).Type <> Type.Empty) Then
                    Grid.SetColumn(pieceSet(x, y).Button, y)
                    Grid.SetRow(pieceSet(x, y).Button, x)
                End If
            Next
        Next
    End Sub

    '------------------------------------------------------------------------
    '                              Piece Logic
    '------------------------------------------------------------------------

    Public Sub MvPiece(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs)
        Dim wPieces = GetButtonsW()
        Dim bPieces = GetButtonsB()

        Dim blankes As Button() = getAvailableMoves()

        Dim Signature = 1

        Dim count = 0
        Dim c1 = 1
        Dim c2 = 1
        Dim c3 = 1
        Dim c4 = 1
        Dim refRow As Integer
        Dim refCol As Integer

        For x = 0 To blankes.Length - 1
            If blankes(x) IsNot Nothing Then
                blankes(x).Visibility = Visibility.Hidden
            End If
        Next

        For x = 0 To 7
            For y = 0 To 7
                If ReferenceEquals(sender, gameBoard(x, y).Button) Then
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

        End If
        '--------------------------------------------QUEEN-----------------------------------------
        If (selected.Type = Type.Queen) Then

        End If
        '--------------------------------------------BISHOP----------------------------------------
        If (selected.Type = Type.Bishop) Then

        End If
        '--------------------------------------------KNIGHT----------------------------------------
        If (selected.Type = Type.Knight) Then

        End If
        '---------------------------------------------ROOK-----------------------------------------
        If (selected.Type = Type.Rook) Then
            '----------------White: Up - Black: Down------------------
            While ((refRow + (c1 * Signature) <> 8) And (refRow + (c1 * Signature) <> -1))
                If (gameBoard(refRow + (c1 * Signature), refCol).Type = Type.Empty Or gameBoard(refRow + (c1 * Signature), refCol).Team <> selected.Team) Then
                    blankes(count).Visibility = Visibility.Visible
                    Grid.SetRow(blankes(count), refRow + (c1 * Signature))
                    Grid.SetColumn(blankes(count), refCol)
                    count += 1
                    If gameBoard(refRow + (c1 * Signature), refCol).Type <> Type.Empty And gameBoard(refRow + (c1 * Signature), refCol).Team <> selected.Team Then
                        Exit While
                    End If
                    c1 += 1
                Else
                    Exit While
                End If
            End While
            '----------------White: Right - Black: Left------------------
            While ((refCol + (c3 * Signature) <> 8) And (refCol + (c3 * Signature) <> -1))
                If (gameBoard(refRow, (c3 * Signature) + refCol).Type = Type.Empty Or gameBoard(refRow, (c3 * Signature) + refCol).Team <> selected.Team) Then
                    blankes(count).Visibility = Visibility.Visible
                    Grid.SetRow(blankes(count), refRow)
                    Grid.SetColumn(blankes(count), refCol + (c3 * Signature))
                    count += 1

                    If gameBoard(refRow, (c3 * Signature) + refCol).Type <> Type.Empty And gameBoard(refRow, (c3 * Signature) + refCol).Team <> selected.Team Then
                        Exit While
                    End If
                    c3 += 1
                Else
                    Exit While
                End If
            End While
            '----------------White: Down - Black: Up------------------
            While ((refRow - (c2 * Signature) <> 8) And (refRow - (c2 * Signature) <> -1))
                If (gameBoard(refRow - (c2 * Signature), refCol).Type = Type.Empty Or gameBoard(refRow - (c2 * Signature), refCol).Team <> selected.Team) Then
                    blankes(count).Visibility = Visibility.Visible
                    Grid.SetRow(blankes(count), refRow - (c2 * Signature))
                    Grid.SetColumn(blankes(count), refCol)
                    count += 1
                    If gameBoard(refRow - (c2 * Signature), refCol).Type <> Type.Empty And gameBoard(refRow - (c2 * Signature), refCol).Team <> selected.Team Then
                        Exit While
                    End If
                    c2 += 1
                Else
                    Exit While
                End If
            End While
            '----------------White: Left - Black: Right------------------
            While ((refCol - (c4 * Signature) <> 8) And (refCol - (c4 * Signature) <> -1))
                If (gameBoard(refRow, refCol - (c4 * Signature)).Type = Type.Empty Or gameBoard(refRow, refCol - (c4 * Signature)).Team <> selected.Team) Then
                    blankes(count).Visibility = Visibility.Visible
                    Grid.SetRow(blankes(count), refRow)
                    Grid.SetColumn(blankes(count), refCol - (c4 * Signature))
                    count += 1
                    If gameBoard(refRow, refCol - (c4 * Signature)).Type <> Type.Empty And gameBoard(refRow, refCol - (c4 * Signature)).Team <> selected.Team Then
                        Exit While
                    End If
                    c4 += 1
                Else
                    Exit While
                End If
            End While
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
            If (gameBoard(row, col).Team = Turn.Black) Then
                gameObject.pointsWhite += 1
            Else
                gameObject.pointsBlack += 1
            End If

            gameBoard(row, col).Type = Type.Empty
            gameBoard(row, col).Button.Visibility = Visibility.Hidden
        End If

        For x = 0 To 7
            For y = 0 To 7
                If (gameBoard(x, y).Type = Type.Empty) And (gameBoard(x, y).Button.IsVisible = True) Then
                    gameBoard(x, y).Button.Visibility = Visibility.Collapsed
                End If
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
