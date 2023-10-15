#include "dlx.h"
#include <cmath>
#include <chrono>
#include <algorithm>
#include <fstream>
#include <map>
#include<set>
#include <numeric>
using namespace std;


struct Cell {
  Cell(int i, int j, int value)
    :i{i}, j{j}, value{value} {}
    
  Cell()
    :i{0}, j{0}, value{0} {}

  
  int i, j, value;
  bool secondary;
};

vector<int> areas;
vector<int> areas_offsets;
vector<vector<pair<int, int>>> matrix;
vector<vector<int>> bin_matrix;
vector<Cell> cells;
vector<vector<pair<int, char>>> adj;
vector<bool> sec;

bool validate(vector<vector<int>>& ans) {
  for (int i = 0; i < ans.size(); ++i) {
    vector<int> vec(ans[0].size(), 0);
    for (int j = 0; j < ans[0].size(); ++j)
      vec[ans[i][j]-1]++;
    
    if (!all_of(vec.begin(), vec.end(), [](const int& n){return n == 1;}))
      return false;
  }
  
  for (int j = 0; j < ans[0].size(); ++j) {
    vector<int> vec(ans.size(), 0);
    for (int i = 0; i < ans.size(); ++i)
      vec[ans[i][j]-1]++;
    
    if (!all_of(vec.begin(), vec.end(), [](const int& n){return n == 1;}))
      return false;
  }

  map<int, set<int>> regions;
  for (int i = 0; i < ans.size(); ++i)
    for (int j = 0; j < ans[0].size(); ++j)
      regions[matrix[i][j].second].insert(ans[i][j]);

  for (auto&[k, v] : regions)
    if (v.size() != areas[k]) return false;

  // for (int i = 0; i < ans.size(); ++i) {
  //   for (int j = 0; j < ans[0].size(); ++j) {
  //     int idx = i*ans.size() + j;
  //     for (auto& c : adj[idx]) {
  //       char d = c.second;

  //       if (d == '>' && ans[i][j] <= ans[i][j+1])
  //         return false;
  //       else if (d == '<' && ans[i][j] >= ans[i][j+1])
  //         return false;
  //       else if (d == 'v' && ans[i][j] <= ans[i+1][j])
  //         return false;
  //       else if (d == '^' && ans[i][j] >= ans[i+1][j])
  //         return false;
  //     }
  //   }
  // }  

  return true;
}




void read_file(const string& path) {
  ifstream file(path);

  int size, depth;
  file.ignore(1024, file.widen(' '));
  file >> size;

  depth = size;

  matrix.assign(size, vector<pair<int,int>>(depth));

  for (int i = 0; i < size; ++i) {
    for (int j = 0; j < depth; ++j) {
      int reg;
      file >> reg;
      reg--;
      matrix[i][j] = {0, reg};
      
      if (reg >= areas.size()) {
        areas.resize(reg+1,0);
      }

      areas[reg]++;
    }
  }


  
  int region_size = sqrt(size);
  for (int i = 0; i < size; ++i) {
    for (int j = 0; j < size - 1; ++j) {
      if (matrix[i][j].second == matrix[i][j+1].second) {
        string d; file >> d;
        int idx = i*size + j;
        
        if (idx >= adj.size())
          adj.resize(idx+1);
        adj[idx].emplace_back(j, d[0]);
      }
    }
  }

  for (int i = 0; i < size - 1; ++i) {
    for (int j = 0; j < size; ++j) {
      if (matrix[i][j].second == matrix[i+1][j].second) {
        string d; file >> d;
        int idx = i*size + j;
        if (idx >= adj.size())
          adj.resize(idx+1);
        adj[idx].emplace_back(j, d[0]);
      }
    }
  }

  int curr = 0;

  for (auto& n : areas) {
    areas_offsets.push_back(curr);
    curr += n;
  }

}

void create_binary_matrix() {
  int num_rows = 0;
  for (auto& n : areas)
    num_rows += n*n;
  
  // num_cols = constraint 1 + constraint 2 + constraint 3 + constraint 4
  // constraint 1 = size*depth
  // constraint 2 = 2 * ( sum([area*area for area in areas]) )
  // constraint 3 = sum(areas)
  // constraint 4 = sum([area*area for area in areas]) 

  int N = matrix.size();
  int M = matrix[0].size();
  int sum_areas = accumulate(areas.begin(), areas.end(), 0);
  int squared_areas = num_rows;
  cout << "sum_areas: " << sum_areas << '\n';
  cout << "sq_areas: " << squared_areas << '\n';
  int num_cols = N*M + sum_areas + N*areas[0] + N*areas[0] + 2*squared_areas;
  cout << "num_cols: " << num_cols << '\n';
  bin_matrix.assign(num_rows, vector<int>(num_cols, 0));
  cells.assign(num_rows, Cell());
  sec.assign(num_cols,false);


  //constraint 1
  //preenche cada célula pertencente a uma regia R com 1 em areas[R] linhas
  int j = 0;
  int counter = 0;
  pair<int, int> curr_cell = {0,0};
  for (int i = 0; i < num_rows; ++i) {
    bin_matrix[i][j] = 1;
    counter++;

    cells[i].i = curr_cell.first;
    cells[i].j = curr_cell.second;
    cells[i].value = counter;
    if (counter >= areas[matrix[curr_cell.first][curr_cell.second].second]) {
      j++;
      counter = 0;

      if (curr_cell.second >= M - 1)
        curr_cell = {curr_cell.first + 1, 0};
      else
        curr_cell.second++;
    }
  }


  // // constraint 2
  int base_offset = N*M;
  int offset = base_offset;
  int curr_row = 0;
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < M; ++j) {
      int curr_area = areas[0];
      int curr_offset = base_offset + areas_offsets[matrix[i][j].second];
      for (int k = 0; k < curr_area; ++k, curr_row++) {
        bin_matrix[curr_row][curr_offset + k] = 1;
      }
      
      offset = curr_offset + curr_area;
    }
  }

  // constraint 3

  curr_row = 0;
  int row_offset = offset;
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < M; ++j) {
      for (int k = 0; k < areas[0]; ++k, ++curr_row) {
        bin_matrix[curr_row][row_offset + k] = 1;
      }
    }
    offset = row_offset + areas[0];
    row_offset += N;
  }

  curr_row = 0;
  base_offset  = offset;
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < M; ++j) {
      for (int k = 0; k < areas[0]; ++k, curr_row++) {
        bin_matrix[curr_row][base_offset + j*areas[0] + k] = 1;
      }

      offset = base_offset + j*areas[0] + areas[0];
    }
  }

  int old_offset = offset;
  while (offset != num_cols) {
    // sec[offset++] = true;
    vector<int> tmp(num_cols,0);
    tmp[offset++] = 1;
    bin_matrix.emplace_back(tmp);
  }
  offset = old_offset;

  curr_row = 0;
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < M; ++j) {
      int idx = i*N + j;
      for (int k = 0; k < areas[0]; ++k, curr_row++) {
        bin_matrix[curr_row][offset + k] = 1;
        
        if (j == M - 1 || matrix[i][j+1].second != matrix[i][j].second || adj.size() <= idx  || !adj[idx].size()) continue;

        char d = '\0';
        for (auto& c : adj[idx]) {
          if (c.second == '<' || c.second == '>') {
            d = c.second;
            break;
          }
        }
        int next_offset = offset + areas[0];

        if (d == '>') {
          for (int l = k; l < areas[0]; ++l)
            bin_matrix[curr_row][next_offset + l] = 1;
        } else if (d == '<') {
          for (int l = k; l >= 0; --l) {
            bin_matrix[curr_row][next_offset + l] = 1;
          }
        }
      }

      offset += areas[0];
    }
  }
  
  curr_row = 0;
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < M; ++j) {
      int idx = i*N + j;
      for (int k = 0; k < areas[0]; ++k, curr_row++) {
        bin_matrix[curr_row][offset + k] = 1;
        
        if (i == N - 1 || matrix[i+1][j].second != matrix[i][j].second || adj.size() <= idx  || !adj[idx].size()) continue;

        char d = '\0';
        for (auto& c : adj[idx]) {
          if (c.second == '^' || c.second == 'v') {
            d = c.second;
            break;
          }
        }

        int next_offset = offset + areas[0]*M;

        if (d == 'v') {
          for (int l = k; l < areas[0]; ++l)
            bin_matrix[curr_row][next_offset + l] = 1;
        } else if (d == '^') {
          for (int l = k; l >= 0; --l) {
            bin_matrix[curr_row][next_offset + l] = 1;
          }
        }
      }

      offset += areas[0];
    }
  }
}

void print_bin_matrix() {
  for (int i = 0; i < bin_matrix.size(); ++i) {
    // cout << cells[i].value << '(' << cells[i].i << ',' << cells[i].j << ") --> ";
    for (auto& m : bin_matrix[i])
      cout << m;
    cout << '\n';
  }
}


int main(int argc, char *argv[]) {
  // if (argc < 2) {
  //   cout << "Uso: main <input>\n";
  //   exit(0);
  // }

  // read_file(argv[1]);
  read_file("input_sudoku");
  create_binary_matrix();
  
  if (argc > 2) {
    print_bin_matrix();
    return 0;
  }

  // using namespace chrono;
  DLX d(bin_matrix, sec);
  // vector<vector<int>> b = {
  //   {1,0,0,0},
  //   {0,0,0,0},
  //   {0,0,0,0},
  //   {1,0,1,0}
  // };
  // vector<bool> s =  {false,false,false,false};
  // DLX d(b, s);
  if (!(d.search(0))) {
    cout << "NO SOLUTION\n";
    return 0;
  }
  auto solutions  = d.solutions;
  // for (auto& r : solutions)
  // cout << r << '\n';

  sort(solutions.begin(), solutions.end());

  vector<vector<int>> ans(matrix.size(), vector<int>(matrix[0].size(), 0));
  for (auto& r : solutions) {
    if (r == -1 || r >= cells.size()) continue;

    auto cell = cells[r];
    ans[cell.i][cell.j] = cell.value;
  }

  for (auto& r : ans) {
    for (auto& n : r)
      cout << n << ' ';
    cout << '\n';
  }
  // if (validate(ans))
  //   cout << "Validation: OK\n";
  // else
  //   cout << "Validation: NOT OK\n";

}
