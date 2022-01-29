Attribute VB_Name = "Module2"
Function FizzBuzz(start, finish)
    n = finish - start + 1
    ReDim charvec(0 To n - 1)
    For i = 0 To n - 1
        If (i + 1) Mod 15 = 0 Then
            charvec(i) = "fizzbuzz"
        ElseIf (i + 1) Mod 5 = 0 Then
            charvec(i) = "buzz"
        ElseIf (i + 1) Mod 3 = 0 Then
            charvec(i) = "fizz"
        Else
            charvec(i) = i + 1
        End If
        Cells(1 + i, 1) = 1 + i
        Cells(1 + i, 2) = charvec(i)
        
    Next i
    FizzBuzz = charvec
    
End Function

Function PriceBond(y, face, couponRate, m)
cf = face * couponRate
pvcfsum = 0
For t = 1 To m
    If t = m Then
    cf = cf + face
    End If
    pvm = (1 + y) ^ (-t)
    pvcf = pvm * cf
    pvcfsum = pvcfsum + pvcf
Next t

PriceBond = pvcfsum

End Function

Sub vecMatMult()

   Dim mat(2, 2) As Integer
   Dim vec(2) As Integer

   For i = 0 To 2
       For j = 0 To 2
           mat(i, j) = i * 3 + j + 1
       Next j
   Next i

   For i = 0 To 2
       vec(i) = i + 1
   Next i

   out = MyMatMult(vec, mat)
  
End Sub

Function MyMatMult(vec, mat)

    Dim a(2) As Integer
    For i = 0 To 2
        a(i) = 0
        For j = 0 To 2
            a(i) = a(i) + vec(j) * mat(j, i)
        Next j
    Next i
    
    MyMatMult = a

End Function


