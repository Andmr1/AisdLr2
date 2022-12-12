#include <iostream>
#include <complex>
#include <math.h>
#include <typeinfo>
using namespace std;
template <typename T>
class matrix
{
	T** _data;
	size_t _columns, _strings;
public:
	friend ostream& operator << <T>(ostream& s, matrix a);
	matrix(size_t columns = 0, size_t strings = 0, T val = 0)
	{
		if (columns < 0 || strings < 0)
		{
			throw("invalid arguments");
		}
		_columns = columns;
		_strings = strings;
		_data = new T* [_strings];
		for (size_t i = 0; i < _strings; i++)
		{
			_data[i] = new T[_columns];
		}
		for (size_t i = 0; i < _strings; i++)
		{
			for (size_t j = 0; j < _columns; j++)
			{
				_data[i][j] = val;
			}
		}
	}
	~matrix()
	{
		for (size_t i = 0; i < _strings; ++i)
		{
			delete[] _data[i];
		}
		delete[] _data;
	}
	matrix(const matrix& mat)
	{
		_columns = mat._columns;
		_strings = mat._strings;
		_data = new T* [_strings];
		for (size_t i = 0; i < _strings; ++i)
		{
			_data[i] = new T[_columns];
		}
		for (size_t i = 0; i < _strings; ++i)
		{
			for (size_t j = 0; j < _columns; ++j)
			{
				_data[i][j] = mat._data[i][j];
			}
		}
	}
	matrix& operator = (const matrix& b)
	{
		if (_data)
		{
			for (size_t i = 0; i < _strings; ++i)
			{
				delete[] _data[i];
			}
			delete[] _data;
		}
		if (b._data)
		{
			_strings = b._strings;
			_columns = b._columns;
			_data = new T* [_strings];
			for (size_t i = 0; i < _strings; ++i)
			{
				_data[i] = new T[_columns];
			}
			for (size_t i = 0; i < _strings; ++i)
			{
				for (size_t j = 0; j < _columns; ++j)
				{
					_data[i][j] = b._data[i][j];
				}
			}
		}
		else
		{
			_data = nullptr;
		}
		return *this;
	}
	T& operator () (size_t strings, size_t columns)
	{
		if (strings > _strings - 1 || strings<0 || columns> _columns - 1 || columns < 0)
		{
			throw("Invalid index");
		}
		return _data[strings][columns];
	}
	matrix operator + (const matrix& a)
	{
		matrix b = *this;
		if (_strings != a._strings || _columns != a._columns)
		{
			throw("invalid sizes");
		}
		for (size_t i = 0; i < _strings; ++i)
		{
			for (size_t j = 0; j < _columns; ++j)
			{
				b._data[i][j] = b._data[i][j] + a._data[i][j];
			}
		}
		return b;
	}
	matrix& operator +=(const matrix& b)
	{
		if (_strings != b._strings || _columns != b._columns)
		{
			throw("Invalid sizes");
		}
		for (size_t i = 0; i < _strings; ++i)
		{
			for (size_t j = 0; j < _columns; ++j)
			{
				this->_data[i][j] = this->_data[i][j] + b._data[i][j];
			}
		}
		return *this;
	}
	matrix operator - (const matrix& a)
	{
		matrix b = *this;
		if (_strings != a._strings || _columns != a._columns)
		{
			throw("invalid sizes");
		}
		for (size_t i = 0; i < _strings; ++i)
		{
			for (size_t j = 0; j < _columns; ++j)
			{
				b._data[i][j] -= a._data[i][j];
			}
		}
		return b;
	}
	matrix operator / (double val)
	{
		matrix b = *this;
		if (val == 0.0)
		{
			throw("division by zero");
		}
		for (size_t i = 0; i < _strings; ++i)
		{
			for (size_t j = 0; j < _columns; ++j)
			{
				b._data[i][j] /= val;
			}
		}
		return b;
	}
	matrix operator * (T val)
	{
		matrix b = *this;
		for (size_t i = 0; i < _strings; ++i)
		{
			for (size_t j = 0; j < _columns; ++j)
			{
				b._data[i][j] *= val;
			}
		}
		return b;
	}
	matrix operator *(matrix a)
	{
		if (_columns != a._strings)
		{
			throw("Invalid sizes");
		}
		matrix c(_strings, a._columns, 0);
		for (size_t i = 0; i < _strings; ++i)
		{
			for (size_t j = 0; j < a._columns; ++j)
			{
				for (size_t k = 0; k < _columns; ++k)
				{
					c._data[i][j] += _data[i][k] * a._data[k][j];
				}
			}
		}
		return c;
	}
	T trail()
	{
		if (_strings != _columns)
		{
			throw("The matrix is not square");
		}
		T tr = 0;
for (size_t i = 0; i < _strings; ++i)
{
	tr += _data[i][i];
}
return tr;
	}
	void inversion()
	{
		T temp;
		T** E = new T * [_strings];
		for (int i = 0; i < _strings; i++)
			E[i] = new T[_strings];
		for (int i = 0; i < _strings; i++)
			for (int j = 0; j < _strings; j++)
			{
				E[i][j] = 0;
				if (i == j)
					E[i][j] = 1;
			}
		for (int k = 0; k < _strings; k++)
		{
			temp = _data[k][k];
			for (int j = 0; j < _strings; j++)
			{
				_data[k][j] /= temp;
				E[k][j] /= temp;
			}
			for (int i = k + 1; i < _strings; i++)
			{
				temp = _data[i][k];
				for (int j = 0; j < _strings; j++)
				{
					_data[i][j] -= _data[k][j] * temp;
					E[i][j] -= E[k][j] * temp;
				}
			}
		}
		for (int k = _strings - 1; k > 0; k--)
		{
			for (int i = k - 1; i >= 0; i--)
			{
				temp = _data[i][k];

				for (int j = 0; j < _strings; j++)
				{
					_data[i][j] -= _data[k][j] * temp;
					E[i][j] -= E[k][j] * temp;
				}
			}
		}
		for (int i = 0; i < _strings; i++)
			for (int j = 0; j < _strings; j++)
				_data[i][j] = E[i][j];
		for (int i = 0; i < _strings; i++)
			delete[] E[i];
		delete[] E;
	}
	void m_new()
	{
		if (_data)
		{
			for (size_t i = 0; i < _strings; ++i)
			{
				delete[] _data[i];
			}
			delete[] _data;
		}
		cout << "strings: ";
		cin >> _strings;
		cout << "columns: ";
		cin >> _columns;
		_data = new T * [_strings];
		for (size_t i = 0; i < _strings; ++i)
		{
			_data[i] = new T[_columns];
		}
		for (size_t i = 0; i < _strings; ++i)
		{
			for (size_t j = 0; j < _columns; ++j)
			{
				cout << "data[" << i << "][" << j << "]: ";
				cin >> _data[i][j];
			}
		}
	}
	bool operator == (const matrix& b)const
	{
		bool c = true;
		if (_strings != b._strings || _columns != b._columns)
		{
			return false;
		}
		for (size_t i = 0; i < _strings; ++i)
		{
			for (size_t j = 0; j < _columns; ++j)
			{
				if (_data[i][j] != b._data[i][j])
				{
					c = false;					
				}
			}
		}
		return c;
	}
};
template<typename T>
ostream& operator <<(ostream& s, matrix<T> a)
{
	for (size_t i = 0; i < a._strings; ++i)
	{
		for (size_t j = 0; j < a._columns; ++j)
		{
			s << a(i,j) << " ";
		}
		s << "\n";
	}
	return s;
}
template<typename T>
matrix<T> operator * (T val,  matrix<T>& a)
{
	return a * val;
}
void menu()
{
	cout << "create matrix:1 \n";
	cout << "trail:2 \n";
	cout << "matrix inversion:3 \n";
	cout << "matrix sum:4 \n";
	cout << "matrix difference:5 \n";
	cout << "matrix multiplication:6 \n";
	cout << "matrix multiplication 2:7 \n";
	cout << "matrix division:8 \n";
	cout << "call element:9 \n";
	cout << "back:10 \n";
	cout << "matrix compirision:11 \n";
}
int main()
{	
	matrix<int> a1(0, 0, 0), b1(0, 0, 0);
	matrix<float> a2(0, 0, 0), b2(0, 0, 0);
	matrix<double> a3(0, 0, 0), b3(0, 0, 0);
	matrix<complex<float>> a4(0, 0, 0), b4(0, 0, 0);
	matrix<complex<double>> a5(0, 0, 0), b5(0, 0, 0);
	bool x = 0, y = 0, e = 0, b = 0, m = 0, n = 0;	

	while (!x)
	{
		std::cout << "Choose type:" << endl;
		std::cout << "int:1" << endl;;
		std::cout << "float:2" << endl;;
		std::cout << "double:3" << endl;;
		std::cout << "std::complex<float>:4" << endl;;
		cout << "std::complex<double>:5" << endl;;
		cout << "quit:6" << endl;;
		size_t c;
		cin >> c;
		int val1 = 0;
		float val3 = 0;
		complex<double> val4(0, 0);
		complex<float> val(0, 0);
		float val6 = 0.0;
		double val2 = 0.0;
		switch (c)
		{
			case 1:
				y = 0;
				while (!y)
				{
					int g;
					menu();
					cin >> g;
					switch (g)
					{
					case 1:
						a1.m_new();
						break;
					case 2:
						cout << a1.trail();
						break;
					case 3:
						cout << "Unavalible for integer";
						//a1.inversion();
						//cout << a1;
						break;
					case 4:
						b1.m_new();
						try
						{
							a1 = a1 + b1;
							cout << a1;
						}
						catch (const char* err)
						{
							cout << err;
						}
						break;
					case 5:
						b1.m_new();
						try
						{
							a1 = a1 - b1;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a1;
						break;
					case 6:
						b1.m_new();
						try
						{
							a1 = a1 * b1;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a1;
						break;
					case 7:
						int c;
						cout << "input number\n";
						cin >> c;
						a1 = a1 * c;
						a1 = c * a1;
						cout << a1;
						break;
					case 8:
						cout << "input number\n";
						cin >> c;
						try
						{
							a1 = a1 / c;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a1;
						break;
					case 9:
						size_t st, col;
						cout << "string\n";
						cin >> st;
						cout << "column\n";
						cin >> col;
						cout << a1(st, col);
						break;
					case 10:
						y = 1;
						break;
					case 11:
						b1.m_new();
						cout << a1.operator==(b1);
						break;
					}			
					cout << "\n\n";
				}
				break;
			case 2:
				e = 0;
				while (!e)
				{
					int g;
					menu();
					cin >> g;
					switch (g)
					{
					case 1:
						a2.m_new();
						break;
					case 2:
						cout << a2.trail();
						break;
					case 3:
						a2.inversion();
						cout << a2;
						break;
					case 4:
						b2.m_new();
						try
						{
							a2 = a2 + b2;
							cout << a2;
						}
						catch (const char* err)
						{
							cout << err;
						}
						break;
					case 5:
						b2.m_new();
						try
						{
							a2 = a2 - b2;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a2;
						break;
					case 6:
						b2.m_new();
						try
						{
							a2 = a2 * b2;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a2;
						break;
					case 7:
						float c;
						cout << "input number\n";
						cin >> c;
						a2 = a2 * c;
						a2 = c * a2;
						cout << a2;
						break;
					case 8:
						cout << "input number\n";
						cin >> val2;
						try
						{
							a2 = a2 / val2;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a2;
						break;
					case 9:
						size_t st, col;
						cout << "string\n";
						cin >> st;
						cout << "column\n";
						cin >> col;
						cout << a2(st, col);
						break;
					case 10:
						e = 1;
						break;
					case 11:
						b2.m_new();
						cout << a2.operator==(b2);
						break;
					}
					cout << "\n\n";
				}
				break;
			case 3:
				b = 0;
				while (!b)
				{
					int g;
					menu();
					cin >> g;
					switch (g)
					{
					case 1:
						a3.m_new();
						break;
					case 2:
						cout << a3.trail();
						break;
					case 3:
						a3.inversion();
						cout << a3;
						break;
					case 4:
						b3.m_new();
						try
						{
							a3 = a3 + b3;
							cout << a3;
						}
						catch (const char* err)
						{
							cout << err;
						}
						break;
					case 5:
						b3.m_new();
						try
						{
							a3 = a3 - b3;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a3;
						break;
					case 6:
						b3.m_new();
						try
						{
							a3 = a3 * b3;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a3;
						break;
					case 7:
						double c;
						cout << "input number\n";
						cin >> c;
						a3 = a3 * c;
						a3 = c * a3;
						cout << a3;
						break;
					case 8:
						cout << "input number\n";
						cin >> val2;
						try
						{
							a3 = a3 / val2;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a3;
						break;
					case 9:
						size_t st, col;
						cout << "string\n";
						cin >> st;
						cout << "column\n";
						cin >> col;
						cout << a3(st, col);
						break;
					case 10:
						b = 1;
						break;
					case 11:
						b3.m_new();
						cout << a3.operator==(b3);
						break;
					}
					cout << "\n\n";
				}
				break;
			case 4:
				m = 0;
				while (!m)
				{
					int g;
					menu();
					cin >> g;
					switch (g)
					{
					case 1:
						a4.m_new();
						break;
					case 2:
						cout << a4.trail();
						break;
					case 3:
						a4.inversion();
						cout << a4;
						break;
					case 4:
						b4.m_new();
						try
						{
							a4 = a4 + b4;
							cout << a4;
						}
						catch (const char* err)
						{
							cout << err;
						}
						break;
					case 5:
						b4.m_new();
						try
						{
							a4 = a4 - b4;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a4;
						break;
					case 6:
						b4.m_new();
						try
						{
							a4 = a4 * b4;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a4;
						break;
					case 7:					
						cout << "input number\n";
						cin >> val;
						a4 = a4 * val;
						a4 = val * a4;
						cout << a4;
						break;
					case 8:
						cout << "input number\n";
						cin >> val2;
						try
						{
							a4 = a4 / val2;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a4;
						break;
					case 9:
						size_t st, col;
						cout << "string\n";
						cin >> st;
						cout << "column\n";
						cin >> col;
						cout << a4(st, col);
						break;
					case 10:
						m = 1;
						break;
					case 11:
						b4.m_new();
						cout << a4.operator==(b4);
						break;
					}
					cout << "\n\n";
				}
				break;
			case 5:
				n = 0;
				while (!n)
				{
					int g;
					menu();
					cin >> g;
					switch (g)
					{
					case 1:
						a5.m_new();
						break;
					case 2:
						cout << a5.trail();
						break;
					case 3:
						a5.inversion();
						cout << a5;
						break;
					case 4:
						b5.m_new();
						try
						{
							a5 = a5 + b5;
							cout << a5;
						}
						catch (const char* err)
						{
							cout << err;
						}
						break;
					case 5:
						b5.m_new();
						try
						{
							a5 = a5 - b5;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a5;
						break;
					case 6:
						b5.m_new();
						try
						{
							a5 = a5 * b5;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a5;
						break;
					case 7:
						cout << "input number\n";
						cin >> val;
						a5 = a5 * val;
						a5 = val4 * a5;
						cout << a5;
						break;
					case 8:
						cout << "input number\n";
						cin >> val2;
						try
						{
							a5 = a5 / val2;
						}
						catch (const char* err)
						{
							cout << err;
						}
						cout << a5;
						break;
					case 9:
						size_t st, col;
						cout << "string\n";
						cin >> st;
						cout << "column\n";
						cin >> col;
						cout << a5(st, col);
						break;
					case 10:
						n = 1;
						break;
					case 11:
						b5.m_new();
						cout << a5.operator==(b5);
						break;
					}
					cout << "\n\n";
				}
				break;
			case 6:
				x = 1;
				break;
		}
	}
	return 0;
}
