#ifndef DLX_HPP
#define DLX_HPP

#include <ostream>
#include <stack>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

struct DLX {
  DLX(vector<vector<int>>& matrix, vector<vector<bool>>& secondary) {
    this->matrix = matrix;
    this->sec = secondary;
    this->N = matrix.size();
    this->M = matrix[0].size();
    this->solutions.assign(N, -1);

    init_matrix();
  }

  ~DLX() {
    free_nodes();
  }


  struct Node {
    Node(Node *l, Node *r, Node *u, Node *d, Node *h, string i)
      :left(l), right(r), up(u), down(d), header(h), id(i), size(0), num_row{-1} {}
    Node(string i)
      :left(nullptr), right(nullptr), up(nullptr), down(nullptr), header(nullptr), id(i), size(0), num_row{-1} {}
    Node(int i)
      :left(nullptr), right(nullptr), up(nullptr), down(nullptr), header(nullptr), id(), size(0), num_row(i) {}

    friend ostream& operator<<(ostream& os, const Node& node) {
      os << "==================================\n"
        << "ID: " << node.id << '\n'
        << "UP: " << node.up->id << '\n'
        << "DOWN: " << node.down->id << '\n'
        << "LEFT: " << node.left->id << '\n'
        << "RIGHT: " << node.right->id << '\n';

      return os;
    }

    Node *left;
    Node *right;
    Node *up;
    Node *down;
    Node *header;
    string id;
    int size;
    int  num_row;
  };

  Node *root = new Node("Root");
  vector<Node*> nodes = {root};
  vector<Node*> rows;
  vector<vector<int>> matrix;
  int N, M;
  vector<int> solutions;
  vector<vector<bool>> sec;

  void init_matrix() {
    Node *curr = root;
    for (int i = 0; i < M-1; ++i) {
      string id; id.push_back('A' + i);
      Node *nd = new Node(id);
      nodes.push_back(nd);

      nd->left = curr;
      curr->right = nd;
      nd->up = nd;
      nd->down = nd;
      curr = nd;
    }

    curr->right = root;
    root->left = curr;

    string id; id.push_back('A' + M - 1);
    Node *nd = new Node(id);
    nd->up = nd->left = nd->right = nd->down = nd;
    nodes.push_back(nd);


    for (int i = 0; i < N; ++i) {
      Node *last = nullptr;
      Node *first = nullptr;
      rows.push_back(nullptr);
      for (int j = 0; j < M; ++j) {
        if (!matrix[i][j]) continue;
        
        Node *col = nodes[j+1];
        Node *curr = new Node(i);
        curr->id = to_string(i) + ',' + to_string(j);
        if (!first) first = curr;

        nodes.push_back(curr);
        if (!rows.back())
          rows.back() = curr;
        curr->left = last;

        if (last)
          last->right = curr;
        curr->header = col;
        curr->up = col->up;
        curr->down = col;
        col->up->down = curr;
        col->up = curr;
        col->size++;

        last = curr;
      }

      if (!first) continue;
      last->right = first;
      first->left = last;
    }

    
  }


  void free_nodes() {
    for (auto node : nodes)
      delete node;
  }


  void print_solution() {
    for (auto& row : solutions) {
      if (row == -1) break;

      for (auto& n : matrix[row])
        cout << n << '\t';

      cout << '\n';
    }
  }

  void cover(Node *col) {
    col->right->left = col->left;
    col->left->right = col->right;

    for (Node *i = col->down; i != col; i = i->down) {
      for (Node *j = i->right; j != i; j = j->right) {
        j->down->up = j->up;
        j->up->down = j->down;
        j->header->size--;
      }
    }
  }

  void uncover(Node *col) {
    for (Node *i = col->up; i != col; i = i->up) {
      for (Node *j = i->left; j != i; j = j->left) {
        j->header->size++;
        j->down->up = j;
        j->up->down = j;
      }
    }

    col->right->left = col;
    col->left->right = col;
  }



  Node *choose_column() {
    Node *column = root->right;
    Node *ptr = root->right;
    int smallest = 1e9;

    while (ptr != root) {
      if (ptr->size < smallest) {
        column = ptr;
        smallest = ptr->size;
      }
      
      ptr = ptr->right;
    }

    return column;
  }

  bool search(int k) {
    if (root->right == root) {
      cout << "FOUND SOLUTION\n";
      print_solution();
      return true;
    } 

    Node *col = choose_column();
    cover(col);

    for (Node *r = col->down; r != col; r = r->down) {
      solutions[k] = r->num_row;
      
      for (Node *j = r->right; j != r; j = j->right)
        cover(j->header);

      bool found = search(k+1);
      if (found) return true;

      solutions[k] = -1;
      for (Node *j = r->left; j != r; j = j->left)
        uncover(j->header);
    }
    
    uncover(col);
    return false;
  }

};
#endif
