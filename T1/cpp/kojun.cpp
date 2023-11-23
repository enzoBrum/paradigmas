#include "dlx.h"
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
vector<vector<int>> expected;

bool validate(vector<vector<int>>& ans) {
  for (int i = 1; i < ans.size(); ++i) {
    for (int j = 0; j < ans[0].size(); ++j) {
      if (ans[i][j] == ans[i-1][j]) return false;
    }
  }

  for (int i = 0; i < ans.size(); ++i) {
    for (int j = 1; j < ans[0].size(); ++j) {
      if (ans[i][j] == ans[i][j-1]) return false;
    }
  }

  map<int, set<int>> regions;
  for (int i = 0; i < ans.size(); ++i)
    for (int j = 0; j < ans[0].size(); ++j)
      regions[matrix[i][j].second].insert(ans[i][j]);

  for (auto&[k, v] : regions)
    if (v.size() != areas[k]) return false;

  for (int i = 1; i < ans.size(); ++i) {
    for (int j = 0; j < ans[0].size(); ++j) {
      if (ans[i][j] > ans[i-1][j] && matrix[i][j].second == matrix[i-1][j].second)
        return false;
    }
  }

  return true;
}




void read_file(const string& path) {
  ifstream file(path);

  int size, depth;
  file.ignore(1024, file.widen(' '));
  file >> size;
  file.ignore(1024, file.widen(' '));
  file >> depth;

  depth = size;

  file.ignore(1024, file.widen('\n'));
  file.ignore(1024, file.widen('\n'));

  matrix.assign(size, vector<pair<int,int>>(depth));
  expected.assign(size, vector<int>(depth));

  for (int i = 0; i < size; ++i) {
    for (int j = 0; j < depth; ++j) {
      string c;
      file >> c;

      if (c == "-")
        matrix[i][j] = {-1,0};
      else
        matrix[i][j] = { stoi(c), 0};
    }
  }

  file.ignore(1024, file.widen('\n'));
  file.ignore(1024, file.widen('\n'));

  for (int i = 0; i < size; ++i) {
    for (int j = 0; j < depth; ++j) {
      int area_num; 
      file >> area_num;
      area_num--;
      if (areas.size() <= area_num)
        areas.resize(area_num+1);

      areas[area_num]++;
      
      matrix[i][j].second = area_num;
    }
  }

  int curr = 0;

  for (auto& n : areas) {
    areas_offsets.push_back(curr);
    curr += n;
  }

  for (int i = 0; i < size; ++i) {
    for (int j = 0; j < size; ++j) {
      file >> expected[i][j];
    }
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

  int num_cols = N*M + 2*squared_areas + sum_areas + 2*squared_areas;

  bin_matrix.assign(num_rows, vector<int>(num_cols, 0));
  cells.assign(num_rows, Cell());


  // constraint 1
  // preenche cada célula pertencente a uma regia R com 1 em areas[R] linhas
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

  // constraint 2.1
  int offset = N*M;
  int curr_row = 0;
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < M; ++j) {
      for (int k = 0; k < areas[matrix[i][j].second]; ++k, curr_row++) {
        bin_matrix[curr_row][offset + k] = 1;
        
        if (!j) continue;

        int last = areas[matrix[i][j-1].second];
        if (k >= last) continue;

        int first_of_last = offset - last;

        bin_matrix[curr_row][first_of_last + k] = 1;
      }

      offset += areas[matrix[i][j].second];
    }
  }

  for (int j = N*M; j < num_cols; ++j) {
    vector<int> row(num_cols, 0);
    row[j]  =1;
    bin_matrix.push_back(row);
  }


  // constraint 2.2
  curr_row = 0;
  vector<int> last_offset(M, 0);
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < M; ++j) {
      for (int k = 0; k < areas[matrix[i][j].second]; ++k, curr_row++) {
        bin_matrix[curr_row][offset + k] = 1;
        if (!i) continue;

        int last = areas[matrix[i-1][j].second];
        if (k >= last) continue;

        int first_of_last = last_offset[j];

        bin_matrix[curr_row][first_of_last + k] = 1;
      }

      last_offset[j] = offset;
      offset += areas[matrix[i][j].second];
    }
  }

  // constraint 3
  int base_offset = offset;
  curr_row = 0;
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < M; ++j) {
      int curr_area = areas[matrix[i][j].second];
      int curr_offset = base_offset + areas_offsets[matrix[i][j].second];
      for (int k = 0; k < curr_area; ++k, curr_row++) {
        bin_matrix[curr_row][curr_offset + k] = 1;
      }
      
      offset += areas[matrix[i][j].second];
    }
  }

  // constraint 4
  curr_row = 0;
  last_offset.assign(M, 0);
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < M; ++j) {
      for (int k = 0; k < areas[matrix[i][j].second]; ++k, curr_row++) {
        bin_matrix[curr_row][offset + k] = 1;

        if (!i) continue;

        int idx_area = matrix[i-1][j].second;

        if (idx_area != matrix[i][j].second) continue;
        
        int last = areas[idx_area];

        int first_of_last = last_offset[j];
        
        for (int l = 0; l <= k; ++l)
          bin_matrix[curr_row][first_of_last + l] = 1;
      }

      last_offset[j] = offset;
      offset += areas[matrix[i][j].second];
    }
  }


  int i = 0;
  while (i < num_rows) {
    auto& cell = cells[i];
    
    if (matrix[cell.i][cell.j].first == -1) {
      ++i;
      continue;
    };

    for (int k = 0; k < areas[matrix[cell.i][cell.j].second]; ++k) {
      auto& cell_to_erase = cells[i + k];
      if (cell_to_erase.value != matrix[cell.i][cell.j].first)
        for (int j = 0; j < num_cols; ++j) {
          if (bin_matrix[i + k][j])
            bin_matrix[i + k][j] = 0;
      }
    }
    i += areas[matrix[cell.i][cell.j].second];
  }

}

void print_bin_matrix() {
  for (int i = 0; i < bin_matrix.size() && i <cells.size(); ++i) {
    cout << cells[i].value << '(' << cells[i].i << ',' << cells[i].j << ") --> ";
    for (auto& m : bin_matrix[i])
      cout << m;
    cout << '\n';
  }
}


int main(int argc, char *argv[]) {
  if (argc < 2) {
    cout << "Uso: main <input>\n";
    exit(0);
  }

  read_file(argv[1]);
  // read_file("input");
  create_binary_matrix();
  // print_bin_matrix();

  DLX d(bin_matrix);
  d.search(0);
  auto solutions  = d.solutions;

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
  if (validate(ans))
    cout << "Validation: OK\n";
  else
    cout << "Validation: NOT OK\n";

  cout << (expected == ans) << '\n';
}
