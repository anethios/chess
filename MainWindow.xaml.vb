Class MainWindow

    Dim blackTurn As Boolean = False
    Dim turnNumber As Integer = 0



    Public Function getpiecesw()
        Dim wPieces As Image() = {wRook, wKnight, wBishop, wKing, wQueen, wBishop2, wKnight2, wRook2, wp1, wp2, wp3, wp4, wp5, wp6, wp7, wp8}
        Return wPieces
    End Function

    Public Function getpiecesb()
        Dim bPieces As Image() = {bRook, bKnight, bBishop, bQueen, bKing, bBishop2, bKnight2, bRook2, bp1, bp2, bp3, bp4, bp5, bp6, bp7, bp8}
        Return bPieces
    End Function

    Public Function whosTurn()
        Return blackTurn
    End Function

    Public Function getTurn()
        Return turnNumber
    End Function

    Public Sub setTurn(ByVal Number As Integer)
        turnNumber = Number
    End Sub

    Public Sub turn()
        If turnNumber = 0 Then
            move1()
        ElseIf turnNumber = 1 Then
            move2()
        ElseIf turnNumber = 2 Then
            move3()
        ElseIf turnNumber = 3 Then
            move4()
        ElseIf turnNumber = 4 Then
            move5()

        End If
        turnNumber += 1
    End Sub

    Public Sub newGame()
        turnNumber = 0
        Dim wPieces = getpiecesw()
        Dim bPieces = getpiecesb()


        Dim x As Integer
        Dim y As Integer
        Dim z As Integer = 0
        Dim wRow As Integer = 7
        Dim bRow As Integer = 0

        chessGrid.Children.Add(bPieces(10))

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

    End Sub

    Public Sub move1()
        Dim wPieces = getpiecesw()
        Grid.SetRow(wPieces(9), 5)

    End Sub

    Public Sub move2()
        Dim bPieces = getpiecesb()
        Grid.SetRow(bPieces(10), 3)
    End Sub

    Public Sub move3()
        Dim wPieces = getpiecesw()
        Grid.SetRow(wPieces(2), 5)
        Grid.SetColumn(wPieces(2), 0)
    End Sub

    Public Sub move4()
        Dim bPieces = getpiecesb()
        Grid.SetRow(bPieces(13), 3)
    End Sub

    Public Sub move5()
        Dim wPieces = getpiecesw()
        Dim bPieces = getpiecesb()
        Grid.SetRow(wPieces(2), 3)
        Grid.SetColumn(wPieces(2), 2)
        removePiece(10, bPieces)
    End Sub

    Public Sub removePiece(ByVal piece As Integer, ByRef PieceSet As Image())
        chessGrid.Children.Remove(PieceSet(piece))
    End Sub

End Class
