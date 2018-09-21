Class MainWindow

    Dim turnSequence As Integer
    Dim turnDisp As TextBox = turnDisplay
    Dim gameStarted As Integer = 0


    '------------------------------------------------------------------------
    '                           Game Functions
    '------------------------------------------------------------------------

    Public Function getButtonsW()
        Dim wButtons As Button() = {wR, wKn, wB, wK, wQ, wB2, wKn2, wR2, wp1B, wp2B, wp3B, wp4B, wp5B, wp6B, wp7B, wp8B}
        Return wButtons
    End Function

    Public Function getButtonsB()
        Dim bButtons As Button() = {bR, bKn, bB, bQ, bK, bB2, bKn2, bR2, bp1B, bp2B, bp3B, bp4B, bp5B, bp6B, bp7B, bp8B}
        Return bButtons
    End Function

    Public Function getTurnDisp()
        Return turnDisplay
    End Function

    Public Function getTurn()
        Return turnSequence
    End Function

    Public Sub setTurn(ByVal number As Integer)
        turnSequence = number
    End Sub


    '------------------------------------------------------------------------
    '                            Game Initializer
    '------------------------------------------------------------------------

    Public Sub newGame()

        setTurn(0)
        Dim wPieces = getButtonsW()
        Dim bPieces = getButtonsB()

        Dim name = Button.NameProperty

        Dim x As Integer
        Dim y As Integer
        Dim z As Integer = 0
        Dim wRow As Integer = 7
        Dim bRow As Integer = 0

        'Add missing pieces back to the board
        For x = 0 To 15
            If chessGrid.Children.Contains(bPieces(x)) = False Then
                chessGrid.Children.Add(bPieces(x))
            End If
            If chessGrid.Children.Contains(wPieces(x)) = False Then
                chessGrid.Children.Add(wPieces(x))
            End If
        Next

        'Set each piece to it's corresponding starting position
        For x = 0 To 1
            For y = 0 To 7
                Grid.SetRow(wPieces(z), wRow)
                Grid.SetColumn(wPieces(z), y)
                Grid.SetRow(bPieces(z), bRow)
                Grid.SetColumn(bPieces(z), y)
                z += 1
            Next
            wRow -= 1
            bRow += 1
        Next
        simulateTurn()
        '---------------------------------------------------------------------
    End Sub

    '------------------------------------------------------------------------
    '                              Game Logic
    '------------------------------------------------------------------------
    Public Sub simulateTurn()
        'Even though these variables are named the same as their globals,
        'the get functions are absolutely necessary
        Dim turnSequence = getTurn()
        Dim turnD = getTurnDisp()

        If turnSequence Mod 2 = 0 Then
            turnD.Text = "Turn: White"
        ElseIf turnSequence Mod 2 = 1 Then
            turnD.Text = "Turn: Black"
        End If

        turnSequence += 1
        setTurn(turnSequence)
    End Sub




    '------------------------------------------------------------------------


    '------------------------------------------------------------------------
    '                              Piece Logic
    '------------------------------------------------------------------------

    Public Sub mvPiece(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs)
        Dim wPieces = getButtonsW()
        Dim bPieces = getButtonsB()

        'If the button that was clicked is equal to the first button of the white piece set then do...
        If (Button.ReferenceEquals(sender, wPieces(0)) = True) Then
            Grid.SetRow(wPieces(0), 4)
        End If
    End Sub

    Public Sub movePiece(ByVal teamCode As Integer, ByVal pieceNumber As Integer, ByVal destRow As Integer, Optional ByVal destCol As Integer = 20)
        Dim wPieces = getButtonsW()
        Dim bPieces = getButtonsB()


        If teamCode = 0 Then
            Grid.SetRow(wPieces(pieceNumber), destRow)
            If destCol <> 20 Then
                Grid.SetColumn(wPieces(pieceNumber), destCol)
            End If
        ElseIf teamCode = 1 Then
            Grid.SetRow(bPieces(pieceNumber), destRow)
            If destCol <> 20 Then
                Grid.SetColumn(bPieces(pieceNumber), destCol)
            End If
        End If

    End Sub

    Public Sub removePiece(ByVal piece As Integer, ByRef PieceSet As Image())
        chessGrid.Children.Remove(PieceSet(piece))
    End Sub

    '------------------------------------------------------------------------

End Class
