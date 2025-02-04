Imports System.IO

Module Program
    ' Represents the robots from the problem statement.
    Class RobotInfo
        Public Position As (Integer, Integer)
        Public Velocity As (Integer, Integer)

        Public Sub New(position As (Integer, Integer), velocity As (Integer, Integer))
            Me.Position = position
            Me.Velocity = velocity
        End Sub
    End Class

    Function ReadVector(vectorString As String) As (Integer, Integer)
        Dim parts = vectorString.Split(",")

        Dim xPart = Integer.Parse(parts(0))
        Dim yPart = Integer.Parse(parts(1))

        Return (xPart, yPart)
    End Function

    ' Reads the robots from the input file.
    Function ReadInput(path As String) As List(Of RobotInfo)
        Dim lines = File.ReadAllLines(path)

        Dim robotInfos As New List(Of RobotInfo)

        For Each line In lines
            Dim parts = line.Split(" ")

            Dim position = ReadVector(parts(0).Split("=")(1))
            Dim velocity = ReadVector(parts(1).Split("=")(1))

            robotInfos.Add(New RobotInfo(position, velocity))
        Next

        Return robotInfos
    End Function

    ' Custom modulus function since VB's built-in `Mod` operator
    ' returns negative results for negative integers.
    Function Modulus(num As Integer, base As Integer) As Integer
        Return ((num Mod base) + base) Mod base
    End Function

    Sub Main(args As String())
        Dim path = "../../../input.txt"

        Dim robots = ReadInput(path)

        ' Dimensions of the space.
        Dim width = 101
        Dim height = 103

        ' Simulate 100 seconds.
        For _elapsed As Integer = 1 To 100
            ' Move the robots in the space.
            For Each robot In robots
                Dim posX = robot.Position.Item1
                Dim posY = robot.Position.Item2
                Dim velX = robot.Velocity.Item1
                Dim velY = robot.Velocity.Item2

                robot.Position.Item1 = Modulus(posX + velX, width)
                robot.Position.Item2 = Modulus(posY + velY, height)
            Next
        Next

        Dim quadrants = New Integer() {0, 0, 0, 0}

        ' Partition the robots into quadrants, ignoring the ones in between
        ' the quadrants.
        For Each robot In robots
            Dim middleX = width \ 2
            Dim middleY = height \ 2

            Dim x = robot.Position.Item1
            Dim y = robot.Position.Item2

            ' Ignore robots in the middle.
            If x = middleX Or y = middleY Then
                Continue For
            End If

            If x < middleX And y < middleY Then
                quadrants(0) += 1
            ElseIf x < middleX And y > middleY Then
                quadrants(1) += 1
            ElseIf x > middleX And y > middleY Then
                quadrants(2) += 1
            Else
                quadrants(3) += 1
            End If
        Next

        Dim res As Integer = quadrants.Aggregate(Function(acc, v) acc * v)
        Console.WriteLine(res)
    End Sub
End Module
