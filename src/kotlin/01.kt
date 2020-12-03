import java.io.File

// Part 1 ----
fun solveP1(numbers: Array<Int>) : Int? {
    val complementMap = mutableMapOf<Int, Int>()

    for (i in numbers.indices) {
        val number: Int = numbers[i]

        if (complementMap.containsKey(number)) {
            return number * complementMap[number]!!
        }

        val complement: Int = 2020 - number
        complementMap[complement] = number
    }

    return null
}


// Part 2 ----
fun solveP2(numbers: Array<Int>) : Int? {
    numbers.sort()
    val count = numbers.size

    for (i in 0..count) {
        var left = i + 1
        var right = count - 1

        while (left < right) {
            val sum = numbers[left] + numbers[right] + numbers[i]

            if (sum == 2020) {
                return numbers[left] * numbers[right] * numbers[i]
            } else if (sum > 2020) {
                right--
            } else {
                left++
            }
        }
    }
    return null
}


fun main() {
    val numbers = mutableListOf<Int>()
    File("input/01.txt").forEachLine { numbers.add(it.toInt()) }
    println(solveP1(numbers.toTypedArray()))
    println(solveP2(numbers.toTypedArray()))
}