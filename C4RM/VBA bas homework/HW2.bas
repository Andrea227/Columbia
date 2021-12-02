Sub Macro1()

x = InputBox("Enter date as a number?")

'
' Macro1 Macro
'

'
    Range("B2:E2").Select
    Selection.AutoFilter
    ActiveSheet.Range("$B$2:$E$869").AutoFilter Field:=1, Criteria1:= _
        "<" & x, Operator:=xlAnd
End Sub

Function getNamedRange(name)
    x = Range(name).Value
    getNamedRange = x
End Function

Function PriceBond(y, face, couponRate, m, Optional ppy = 1)

    cf = face * couponRate * (1 / ppy)
    pvcfsum = 0
    n = m - 1
    
    If ppy = 1 Then
        n = m - 1
        For t = 0 To n
            pvm = (1 + y) ^ (-t - 1)
            pvcfsum = pvcfsum + pvm * cf
        Next t
    ElseIf ppy >= 2 Then
        n = m * ppy - 1
        For t = 0 To n
            pvm = (1 + (y / ppy)) ^ (-t - 1)
            pvcfsum = pvcfsum + pvm * cf
        Next t
    End If
    
    pvcfsum = pvcfsum + pvm * face
    
    PriceBond = pvcfsum

End Function

Function MyMatMult(vec, mat)
    
    nrow = UBound(vec, 1)
    ncol = UBound(mat, 2)
    ReDim out(ncol) As Variant
    
     For i = 0 To ncol
        rowsum = 0
        For j = 0 To nrow
            rowsum = rowsum + vec(j) * mat(j, i)
        Next j
        out(i) = rowsum
    Next i

    MyMatMult = out

End Function

Function MyTripDataObj()
    
    Dim d As Variant
    d = Range("C38:Z48").Value
    
    Dim x(11, 2, 3, 4) As Double
    
    For i_person = 0 To 3
        For i_trip = 0 To 2
            For i_step = 0 To 10
                For i_latlong = 0 To 1
                    x(i_step, i_latlong, i_trip, i_person) = d(i_step, i_latlong + i_person * 2 + i_trip * 8)
                Next i_latlong
            Next i_step
        Next i_trip
    Next i_person
    
    
    MyTripDataObj = x
End Function

Function FizzBuzz(start, finish)

    Dim n As Integer
    n = finish - start + 1
    
    ReDim charvec(0 To n - 1) As String
    
    Dim i As Integer
    Dim x As Double
          
    For i = 0 To n - 1
        x = i + start
        
        charvec(i) = x
        
        If x Mod 15 = 0 Then
            charvec(i) = "fizzbuzz"
        ElseIf x Mod 5 = 0 Then
            charvec(i) = "buzz"
        ElseIf x Mod 3 = 0 Then
            charvec(i) = "fizz"
        End If
    
    Next i
    
    FizzBuzz = charvec

End Function
