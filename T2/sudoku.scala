import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.immutable
import scala.math.sqrt
import scala.Enumeration
import scala.util.boundary, boundary.break
import scala.languageFeature.postfixOps


object SudokuSolver {
    final var N = 9
    final var sqrtN = sqrt(N).toInt
    
    object Direction extends Enumeration {
        val UP, DOWN, LEFT, RIGHT = Value
    }

    def getRegionNum(i: Int, j: Int): Int = {
        // Retorna o número de uma região.
        /*
            0011
            0011
            2233
            2233
        */
        (i / sqrtN) * sqrtN + j / sqrtN
    }
    
    def readMatrix(path: String): (Array[Int], HashMap[Int, Direction.Value], HashMap[Int, Direction.Value]) = {
        /*
        Lê um sudoku de um arquivo e retorna uma tripla contendo:
            - Matriz lida
            - Map que mapeia uma célula da matriz à uma direção que indica a comparação com uma célula de mesma região à direita
            - Map que mapeia uma célula da matriz à uma direção que indica a comparação com uma célula de mesma região abaixo
        */
        
        val file_source = Source.fromFile(path)
        val matrix = Array.ofDim[Int](N*N)
        val hcmp = HashMap[(Int), Direction.Value]()
        val vcmp = HashMap[(Int), Direction.Value]()
        file_source.getLines().zipWithIndex.foreach{
            case (line: String, i: Int) => {
                for ((num, j) <- line.split(' ').zipWithIndex) {
                    matrix(i*N + j) = num.toInt
                }
            }    
        }
        
        for (i <- 0 until N) {
            for (j <- 0 until N) {
                if (getRegionNum(i, j) == getRegionNum(i, j+1) && j+1 < N) {
                    if (matrix(i * N + j) < matrix(i * N + j + 1))
                        hcmp += (i * N + j -> Direction.LEFT)
                    else
                        hcmp += (i * N + j -> Direction.RIGHT)
                }

                if (getRegionNum(i, j) == getRegionNum(i+1, j) && i+1 < N) {
                    if (matrix(i * N + j) < matrix((i+1) * N + j))
                        vcmp += (i * N + j -> Direction.UP)
                    else
                        vcmp += (i * N + j -> Direction.DOWN)
                }
            }
        }

        (matrix, hcmp, vcmp)
    }


    def updateRegionMap(
        map: immutable.HashMap[Int, immutable.HashSet[Int]],
        dirmap: HashMap[Int, Direction.Value],
        curr_val: Int,
        curr_idx: Int,
        next_idx: Int
    ): immutable.HashMap[Int, immutable.HashSet[Int]] = {
        /*
            Caso curr_dx e next_idx pertençam à mesma região, olha o mapa de direção e remove os elementso de map que são maiores/menores que curr_val
        */
        if (next_idx < N*N && dirmap.contains(curr_idx))
            map.updated(
                next_idx,
                dirmap(curr_idx) match 
                    case Direction.DOWN | Direction.RIGHT => map(next_idx).filter( (i: Int) => i < curr_val )
                    case Direction.UP   | Direction.LEFT => map(next_idx).filter( (i: Int) => i > curr_val )
            )
        else
            map
    }

    def backtrack(
        hcmp: HashMap[Int, Direction.Value],
        vcmp: HashMap[Int, Direction.Value],
        matrix: Array[Int],
        region_posible_values: Array[immutable.HashMap[Int, immutable.HashSet[Int]]],
        lines: Array[HashSet[Int]],
        cols: Array[HashSet[Int]],
        regions: Array[HashSet[Int]],
        param_i: Int = 0,
        param_j: Int = 0
    ): Boolean =  {
        
        // A matriz é percorrida da esquerda para a direita.
        // Se i for N-1, a solução foi encontrada
        if (param_i == N-1 && param_j == N)
            return true

        val (i,j) = 
            if (param_j == N) (param_i+1,0)
            else (param_i,param_j)
        
        
        val index = i*N + j
        val region_index = getRegionNum(i,j)
        val next_right_index = index + 1
        val next_down_index = index + N
        
        // Possíveis valores para a célula atual
        val cell_posible_values = region_posible_values(region_index)(index)
                                    .diff(lines(i))
                                    .diff(cols(j))
                                    .diff(regions(region_index))

        
        val old_region_map = region_posible_values(region_index)
        var solution_found = false
        boundary:
            for (value <- cell_posible_values) {
                val new_region_map = updateRegionMap(
                    updateRegionMap(old_region_map, hcmp, value, index, next_right_index ),
                    vcmp, value, index, next_down_index
                )
                

                region_posible_values(region_index) = new_region_map
                
                lines(i) += value
                cols(j) += value
                regions(region_index) += value

                matrix(index) = value+1
                
                val no_posible_value_right = (j != N-1 && getRegionNum(i,j+1) == region_index && region_posible_values(region_index)(i*N + j + 1).isEmpty)
                val no_posible_value_down = (i != N-1 && getRegionNum(i+1,j) == region_index && region_posible_values(region_index)((i+1)*N + j).isEmpty)
                if (!no_posible_value_right && !no_posible_value_down && backtrack(hcmp, vcmp, matrix, region_posible_values, lines, cols, regions, i, j+1)) {
                    solution_found = true
                    break(true)
                }
                
                regions(region_index) -= value
                cols(j) -= value
                lines(i) -= value

                region_posible_values(region_index) = old_region_map
            }

        solution_found
    }
    
    def solve(
        hcmp: HashMap[Int, Direction.Value], 
        vcmp: HashMap[Int, Direction.Value]): Array[Int] = {
            val matrix = Array.ofDim[Int](N*N) // matriz solução
            val lines = Array.fill(N){ HashSet.empty[Int] } // Cada elemento i corresponde à um set contendo os valores na linha i
            val cols = Array.fill(N){ HashSet.empty[Int] } // Cada elemento i corresponde à um set contendo os valores na coluna i
            val regions = Array.fill(N){ HashSet.empty[Int] } // Cada elemento i corresponde à um set contendo os valores na região i
            
            // mapeia o número de uma região para um set contendo os índices presentes nessa região
            val indexes_per_region = HashMap[Int, HashSet[Int]](
                (0 to N-1).map {key => key -> HashSet.empty[Int]}: _*
            )

            
            for (i <- 0 until N) {
                for (j <- 0 until N) {
                    val region_index = getRegionNum(i,j)
                    indexes_per_region(region_index).add(i*N + j)
                }
            }
            
            val create_map = (mp: immutable.HashMap[Int, immutable.HashSet[Int]], v: Int) => mp + (v -> immutable.HashSet.range(0, N))
            
            // Cria um array cujo cada elemento é um Map representando uma região.
            // O map de uma região R mapeia os índices das células dentro da região R à Sets contendo os valores possíveis de cada elemento em R. 
            val possible_values = Array.tabulate(N) { region_index =>
                indexes_per_region(region_index).foldLeft(immutable.HashMap.empty[Int, immutable.HashSet[Int]])(create_map)
            }

            backtrack(hcmp, vcmp, matrix, possible_values, lines, cols, regions, 0, 0)
            matrix
    }
    
    def main(args: Array[String]): Unit = {
        if (args.length == 0) {
            println("Uso: main <sudoku-file>")
            return
        }
        
        val begin = System.currentTimeMillis()

        val (expected_solution, hcmp, vcmp) = readMatrix(args(0))
        val solution = solve(hcmp, vcmp)

        val duration = System.currentTimeMillis() - begin
        
        if (!solution.sameElements(expected_solution)) {
            println("Solução não encontrada!")
        } else {
            println("Solução encontrada!")
            println(solution.grouped(N).toArray.foreach( arr => {
                arr.foreach( num => print(s"$num ") )
                print('\n')
            }))
    
            println(s"Levou $duration ms")
        }
    }
}