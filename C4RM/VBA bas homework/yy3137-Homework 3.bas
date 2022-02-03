Attribute VB_Name = "Module1"

Option Explicit
Option Base 1

Function PriceBond(y, face, couponRate, m, Optional ppy = 1)

Dim cf As Variant 'cash flow
Dim pvcfsum As Variant 'presnet value of the cash flow sum
Dim pvm As Variant 'present value of each cash flow
Dim t As Integer 'payment terms

cf = face * couponRate * (1 / ppy)
pvcfsum = 0

If ppy = 1 Then
    For t = 1 To m
        pvm = (1 + y) ^ (-t)
        pvcfsum = pvcfsum + pvm * cf
    Next t
ElseIf ppy = 2 Then
    For t = 1 To m * ppy
        pvm = (1 + (y / ppy)) ^ (-t)
        pvcfsum = pvcfsum + pvm * cf
    Next t
End If

pvcfsum = pvcfsum + pvm * face

PriceBond = pvcfsum

End Function

Sub mybondtest()
    Dim y As Double
    Dim face As Double
    Dim couponRate As Double
    Dim m As Integer
    Dim ppy As Integer
    Dim x2 As Variant
    Dim x1 As Variant
    Dim x0 As Variant
    
    y = 0.03
    couponRate = 0.04
    m = 10
    face = 2000000
    ppy = 2
    
    x2 = PriceBond(y, face, couponRate, m, ppy)
    x1 = PriceBond(y, face, couponRate, m, 1)
    x0 = PriceBond(y, face, couponRate, m)
    
    If Round(x2, 0) = 2171686 And Round(x1) = 2170604 And Round(x0, 0) = 2170604 Then
        MsgBox ("Right answer")
    Else
        MsgBox ("Wrong answer")
    End If
    
End Sub

Sub test_MatMult()
    
    Dim n As Integer
    Dim m As Integer
    Dim c As Integer
    Dim r As Integer
    Dim a As Integer
    Dim b As Integer
    Dim nRand As Variant
    Dim p As Double
    
    
    'Rnd is a random draw from 0 to 1
    m = Round(Rnd * 5, 0) + 1
    n = Round(Rnd * 5, 0) + 1
    
    ReDim mat(m, n) As Integer
    ReDim vec(m) As Integer
    
    Dim out2 As Variant
    
    For a = 1 To m
        For b = 1 To n
            mat(a, b) = (a - 1) * 3 + b
        Next b
    Next a
    
    For a = 1 To m
        vec(a) = a
    Next a
    
    out2 = MyMatMult(vec, mat)
    
    nRand = Application.Min(Round(Rnd * 3, 0) + 1, n)
    
    Dim randAns As Double
    For p = 1 To m
        randAns = randAns + vec(p) * mat(p, nRand)
    Next p
    
    If out2(nRand) = randAns Then
        MsgBox ("Right answer")
    Else
        MsgBox ("Wrong answer")
    End If

 End Sub
 
 Function MyMatMult(vec, mat)
    
    Dim nrow As Double
    Dim ncol As Double
    Dim i As Double
    Dim j As Double
    
    nrow = UBound(vec, 1)
    ncol = UBound(mat, 2)
    ReDim out(ncol) As Double
    
    For i = 1 To ncol
        For j = 1 To nrow
            out(i) = out(i) + vec(j) * mat(j, i)
        Next j
    Next i

    MyMatMult = out

End Function

Function MyTripDataObj()

    Dim i_step As Integer
    Dim i_latlong As Integer
    Dim i_trip As Integer
    Dim i_person As Integer
    
    Dim d As Variant
    d = Range("C38:Z48").Value
    
    Dim x(11, 2, 3, 4) As Double
    
    For i_person = 1 To 4
        For i_trip = 1 To 3
            For i_step = 1 To 11
                For i_latlong = 1 To 2
                    x(i_step, i_latlong, i_trip, i_person) = d(i_step, i_latlong + (i_person - 1) + (i_trip - 1) * 8)
                Next i_latlong
            Next i_step
        Next i_trip
    Next i_person
    
    
    MyTripDataObj = x
End Function

Function FizzBuzz(start, finish)

    Dim n As Integer
    n = finish - start + 1
    
    ReDim charvec(1 To n) As String
    
    Dim i As Integer
    Dim x As Double
          
    For i = 1 To n
        x = i + start - 1
        
        charvec(i) = x
        
        If x Mod 15 = 0 Then
            charvec(i) = "fizzbuzz"
        ElseIf x Mod 5 = 0 Then
            charvec(i) = "buzz"
        ElseIf x Mod 3 = 0 Then
            charvec(i) = "fizz"
        End If
        
        Cells(i, 1) = start - 1 + i
        Cells(i, 2) = charvec(i)
    
    Next i
    
    FizzBuzz = charvec

End Function

Sub testfb()
    Dim z As Variant
    z = FizzBuzz(20, 31)
End Sub








