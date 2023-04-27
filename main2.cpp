#include <iostream>
#include <vector>
#include <cmath>
#include <iomanip>


using namespace std;


class Matrix {
public:
    int rows;
    int columns;
    vector<vector<double>> matrix;

    Matrix(int n, int m) {
        matrix = *new vector<vector<double>>(n, vector<double>(m));
        rows = n;
        columns = m;
    }

    Matrix(int n, int m, vector<vector<double>> mat){
        matrix = mat;
        rows = n;
        columns = m;
    }

    void extend(Matrix m){
        for (int i = 0; i < m.rows; i++){
            for (int j = 0; j < m.columns; j++){
                this->matrix[i].push_back(m.matrix[i][j]);
            }
        }
        columns = columns * 2;
    }

    void dextend(){
        for (int i = 0; i < rows; i++){
            for (int j = 0; j < rows; j++){
                matrix[i].erase(matrix[i].end() - 1);
            }
        }
        columns = columns /  2;
    }

    Matrix *transpose() {
        vector<vector<double>> tempmat(columns, vector<double>(rows));
        for (int j = 0; j < columns; j++) {
            for (int i = 0; i < rows; i++) {
                tempmat[j][i] = matrix[i][j];
            }
        }
        return new Matrix(columns, rows, tempmat);
    }

    void operator=(Matrix &mat) {
        matrix = mat.matrix;
        rows = mat.rows;
        columns = mat.columns;
    }


    friend Matrix *operator-(Matrix m1, Matrix m2) {
        if (m1.rows == m2.rows && m1.columns == m2.columns) {
            vector<vector<double>> newVector(m1.rows, vector<double>(m1.columns));
            for (int i = 0; i < m1.rows; i++) {
                for (int j = 0; j < m1.columns; j++) {
                    newVector[i][j] = m1.matrix[i][j] - m2.matrix[i][j];
                }
            }
            return new Matrix(m1.rows, m1.columns, newVector);
        } else {
            vector<vector<double>> emptyVector(0, vector<double>(0));
            cout << "Error: the dimensional problem occurred" << endl;
            return new Matrix(0, 0, emptyVector);
        }
    }

    friend Matrix *operator*(Matrix m1, Matrix m2) {
        if (m1.columns == m2.rows) {
            int newN = m1.rows;
            int newM = m2.columns;
            vector<vector<double>> newVector(newN, vector<double>(newM));
            for (int i = 0; i < m1.rows; i++) {
                for (int j = 0; j < m2.columns; j++) {
                    for (int k = 0; k < m1.columns; k++) {
                        newVector[i][j] += m1.matrix[i][k] * m2.matrix[k][j];
                    }
                }
            }
            return new Matrix(newN, newM, newVector);
        } else {
            vector<vector<double>> emptyVector(0, vector<double>(0));
            cout << "Error: the dimensional problem occurred" << endl;
            return new Matrix(0, 0, emptyVector);
        }
    }

    friend Matrix *operator+(Matrix m1, Matrix m2) {
        if (m1.rows == m2.rows && m1.columns == m2.columns) {
            vector<vector<double>> newVector(m1.rows, vector<double>(m1.columns));
            for (int i = 0; i < m1.rows; i++) {
                for (int j = 0; j < m1.columns; j++) {
                    newVector[i][j] = m1.matrix[i][j] + m2.matrix[i][j];
                }
            }
            return new Matrix(m1.rows, m1.columns, newVector);
        } else {
            vector<vector<double>> emptyVector(0, vector<double>(0));
            cout << "Error: the dimensional problem occurred" << endl;
            return new Matrix(0, 0, emptyVector);
        }
    }

    friend istream &operator>>(istream &inp, Matrix& m) {
        for (auto& i : m.matrix) {
            for (auto& j : i) {
                inp >> j;
            }
        }
        return inp;
    }


    friend ostream &operator<<(ostream &out, Matrix m){
        double elem;
        for (int i = 0; i < m.rows; i++){
            for (int j = 0; j < m.columns; j++){
                elem = round(m.matrix[i][j] * 10000) / 10000;
                if(elem == -0){
                    elem = 0.0;
                }
                out <<  fixed << setprecision(4) << elem << " ";
            }
            out << endl;
        }
        return out;
    }



};

class ColumnVector{
public:
    int rows;
    vector<double> vecto;
    ColumnVector(int n){
        rows = n;
        vecto = *new ::vector<double>(n);
    }

    ColumnVector(int n, ::vector<double> vect){
        rows = n;
        vecto = vect;
    }

    friend istream &operator>>(istream &inp, ColumnVector& vect) {
        for (auto& i : vect.vecto) {
            inp >> i;
        }
        return inp;
    }

    friend ostream &operator<<(ostream &out, ColumnVector vect){
        double elem;
        for (int j = 0; j < vect.rows; j++){
            elem = round(vect.vecto[j] * 10000) / 10000;
//            if (elem == -0){
//                elem = 0.0;
//            }
            out << fixed << setprecision(4) << elem << endl;
        }
        return out;
    }

    friend ColumnVector *operator *(Matrix m, ColumnVector vect){
        ::vector<double> newVect(m.rows);
        double value;
        for (int i = 0; i < m.rows; i++){
            value = 0;
            for (int j = 0; j < m.columns; j++){
                value = value + m.matrix[i][j] * vect.vecto[j];
            }
            newVect[i] = value;
        }
        return new ColumnVector(m.rows, newVect);
    }

};


class IdentityMatrix: public Matrix{
public:
    IdentityMatrix(int n) : Matrix(n, n) {
        for (int i = 0; i < n; i++){
            matrix[i][i] = 1;
        }
    }
};

class EliminationMatrix: public IdentityMatrix{
public:
    EliminationMatrix(int i, int j, Matrix m): IdentityMatrix(m.rows){
        i--;
        j--;
        double coef = m.matrix[i][j] / m.matrix[j][j];
        matrix[i][j] = -coef;
    }
};

class PermutationMatrix: public IdentityMatrix{
public:
    PermutationMatrix(int i, int j, int n): IdentityMatrix(n){
        i --;
        j--;
        vector<double> temp = matrix[i];
        copy(matrix[j].begin(), matrix[j].end(), matrix[i].begin());
        copy(temp.begin(), temp.end(), matrix[j].begin());
    }
};

int swapL(Matrix& m, Matrix& m2,  int column){
    int index = column;
    double Max = abs(m.matrix[column][column]);
    for (int i = column + 1; i < m.columns; i++){
        if (abs(m.matrix[i][column]) > Max){
            Max = abs(m.matrix[i][column]);
            index = i;
        }
    }
    if (index != column){
        PermutationMatrix P(column + 1, index + 1, m.rows);
        Matrix *temp;
        temp = P * m;
        m = *temp;
        temp = P * m2;
        m2 = *temp;

        return 1;
    }
    return 0;
}



void inverseMatrix(Matrix& m){
    Matrix I = *new IdentityMatrix(m.columns);
    int res;
    for (int i = 0; i < m.columns; i++){
        res = swapL(m, I, i);
        for (int j = i + 1; j < m.columns; j++){
            if(m.matrix[j][i] != 0) {
                EliminationMatrix E(j + 1, i + 1, m);
                Matrix *temp;
                temp = E * m;
                m = *temp;
                temp = E * I;
                I = *temp;
            }
        }
    }
    for (int i = m.columns - 1; i > -1; i--) {
        for (int j = i - 1; j > -1; j--) {
            if (m.matrix[j][i] != 0) {
                EliminationMatrix E(j + 1, i + 1, m);
                Matrix *temp;
                temp = E * m;
                m = *temp;
                temp = E * I;
                I = *temp;
            }
        }
    }

    for (int i = 0; i < m.columns; i++){
        for(int j = 0; j < m.columns; j++) {
            I.matrix[i][j] = I.matrix[i][j] / m.matrix[i][i];
        }
        m.matrix[i][i] = 1;
    }

    m = I;
}



int main()
{
    FILE* op = _popen("C:\\gnuplot\\bin\\gnuplot -persist", "w");
    int n;
    cin >> n;
    vector<double> xes(n);
    vector<double> vect(n);
    for(int i = 0; i < n; i++){
        cin >> xes[i];
        cin >> vect[i];
    }
    int dim;
    cin >> dim;
    dim++;
    vector<vector<double>> matr(n, vector<double>(dim));
    for(int i = 0; i < n; i++){
        for(int j=0; j < dim; j++){
            matr[i][j] = pow(xes[i], j);
        }
    }
    Matrix A(n, dim, matr);
    ColumnVector b(n, vect);

    cout << "A:" << endl;
    cout << A;
    Matrix* AT;
    Matrix* R;
    ColumnVector* V;
    AT = A.transpose();
    R = *AT * A;
    cout << "A_T*A:"<< endl;
    cout << *R;
    inverseMatrix(*R);
    cout << "(A_T*A)^-1:" << endl;
    cout << *R;
    V = *AT * b;
    cout << "A_T*b:" << endl;
    if (V->rows < dim){
        while (V->rows != dim){
            V->rows++;
            V->vecto.push_back(0);
        }
    }
    cout << *V;
    ColumnVector *x;
    x = *R * *V;
    if (x->rows < dim){
        while (x->rows != dim){
            x->rows++;
            x->vecto.push_back(0);
        }
    }
    cout << "x~:" << endl;
    cout << *x;
    fprintf(op, "plot [-7 : 10] [-100 : 10] f(x) = %lf*x**3 + %lf*x**2 + %lf*x**1 + %lf*x**0, f(x) title 'f(x)', '-' using 1:1 title 'point' with points\n", x->vecto[0], x->vecto[1], x->vecto[2], x->vecto[3]);
    fflush(op);
    for(int i = 0; i < n; i++){
        cout << xes[i] << " " << vect[i] << endl;
        fprintf(op, "%lf\t%lf\n", xes[i], vect[i]);
    }
}