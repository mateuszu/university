
#include <vector>
#include <string>
#include <cstdio>

using namespace std;

int rozmiarBloku = 18;

// generuje wszystkie mozliwe ustawienia skoczkow na kwadracie 3x3
vector<string> generujWszystkieUstawienia(int n)
{
	vector<string> ustawienia;

	if (n <= 0)
	{
		return ustawienia;
	}
	ustawienia.push_back(".");
	ustawienia.push_back("S");

	int i, j;
	for (i = 2; i < (1 << n); i = i << 1)
	{
		for (j = i - 1; j >= 0; j--)
		{
			ustawienia.push_back(ustawienia[j]);
		}

		for (j = 0; j < i; j++)
		{
			ustawienia[j] = "." + ustawienia[j];
		}

		for (j = i; j < 2 * i; j++)
		{
			ustawienia[j] = "S" + ustawienia[j];
		}
	}


	return ustawienia;
}


// sprawdza czy indeksy sa zawarte w kwadracie 3x3
bool wPlanszy(int r, int c)
{
	return r >= 0 && r < 3 && c >= 0 && c < 3;
}


int policz(string& u)
{
	int ile = 0;
	for (int k = 0; k < rozmiarBloku; k++)
	{
		if (u[k] == 'S') ile++;
	}
	return ile;
}

bool kolizjaZeZniszczonymBlokiem(char* daneWejsciowe, int i, string& u)
{
	// najpierw sprawdzamy czy dane ustawienie
	// nie bedzie kolidowalo z jakims zniszczonym polem
	for (int k = 0; k < rozmiarBloku; k++)
	{
		// jesli skoczek znalazlby sie na zniszczonym polu
		// to nie bedzie to poprawne ustawienie
		if (daneWejsciowe[i + k] == 'x' && u[k] == 'S')
		{
			return true;
		}
	}
	return false;
}



bool nieprawidlowyOstatniBlok(int i, int reszta, int n, string& u)
{
	if (i + rozmiarBloku > n * 3)
	{
		if (reszta != 0)
		{
			for (int r = reszta; r < (rozmiarBloku / 3); r++)
			{
				if (u[r * 3] == 'S' || u[r * 3 + 1] == 'S' || u[r * 3 + 2] == 'S')
				{
					return true;
				}
			}
		}
	}
	return false;
}

void wypiszBlok(string& u)
{
	for (unsigned i = 0; i < u.size(); i++)
	{
		putchar(u[i]);
		if (i % 3 == 2) putchar('\n');
	}
	putchar('\n');
}

bool kolizjaZPoprzednimBlokiemm(int i, int reszta, int n, string& aktualny, string& poprzedni)
{
	// sprawdzamy czy aktualne ustawienie nie koliduje z ustawieniem ktore bylo
	// dla poprzedniego kwadratu 3x3
	// np dla ponizszego przypadku jest ok
	// S.S <- poprzedni
	// .S.
	// S.S 
	// .S. <- aktualny
	// S.S
	// .S.
	// a dla tego juz nie
	// S.S <- poprzedni
	// .S.
	// S.S 
	// S.S <- aktualny
	// .S.
	// S.S

	int p = poprzedni.size() - 9;

	if (i != 0)
	{
		if (aktualny[0] == 'S' && (poprzedni[p + 4] == 'S' || poprzedni[p + 8] == 'S')) return true;
		if (aktualny[1] == 'S' && (poprzedni[p + 3] == 'S' || poprzedni[p + 5] == 'S')) return true;
		if (aktualny[2] == 'S' && (poprzedni[p + 4] == 'S' || poprzedni[p + 6] == 'S')) return true;

		if (i + rozmiarBloku < n * 3 && reszta != 1)
		{
			if (aktualny[3] == 'S' && poprzedni[p + 7] == 'S') return true;
			if (aktualny[4] == 'S' && (poprzedni[p + 6] == 'S' || poprzedni[p + 8] == 'S')) return true;
			if (aktualny[5] == 'S' && poprzedni[p + 7] == 'S') return true;
		}
	}
	return false;
}


void dodajDoWyniku(char* daneWejsciowe, vector<string>& wektorWynikowy, int i, string& u)
{
	// dodajemy wybrane ustawienie do danych wynikowych
	// musimy tez wstawic oczywiscie wszystkie pola uszkodzone
	string s = "";
	for (int k = 0; k < rozmiarBloku; k++)
	{
		if (daneWejsciowe[i + k] == 'x') s += 'x';
		else s += u[k];

		if (k % 3 == 2)
		{
			wektorWynikowy.push_back(s);
			s = "";
		}
	}
}



int main()
{
	// wczytanie rozmiaru
	int n;
	scanf("%d", &n);
	 
	// alokacja tablicy na dane wejsciowe
	char* daneWejsciowe = new char[3 * n];
	// wczytanie danych wejsciowych
	int i = 0;
	char c;
	while (i < 3 * n)
	{
		c = getchar();
		if (c == '.' || c == 'x')
		{
			daneWejsciowe[i++] = c;
		}
	}
	
	vector<string> wszystkieUstawienia = generujWszystkieUstawienia(9);

	
	// w U beda tylko te ustawienia gdzie skoczki sie nie szachuja
	vector<string> U3;

	// skoczek moze wykonac 8 ruchow
	// rs - o ile w danym ruchu sie przesuwamy w wierszu
	// cs - o ile w danym ruchu sie przesuwamy w kolumnie
	int rs[] = { -2, -2, -1, -1, 1, 1, 2, 2 };
	int cs[] = {-1, 1, -2, 2, -2, 2, -1, 1};

	// przechodzimy przez wszystkie ustawienia
	for (unsigned i = 0; i < wszystkieUstawienia.size(); i++)
	{
		bool ok = true;
		// sprawdzamy wszystkie pola
		for (int j = 0; j < 9; j++)
		{
			// jesli pole jest puste to nie musimy tego pola sprawdzac
			// i przechodzimy dalej
			if (wszystkieUstawienia[i][j] == '.') continue;

			// wyliczamy numer wiersza i numer kolumny na podstawie indeksu (od 0 do 9)
			int r = j / 3;
			int c = j % 3;

			// sprawdzamy wszystkie mozliwe ruchy skoczka
			for (int d = 0; d < 8; d++)
			{
				// numer wiersza i numer kolumny pola na ktorym skoczek by sie znalazl
				int sasiednieR = r + rs[d];
				int sasiednieC = c + cs[d];
				// upewniamy sie ze to pole bedzie w planszy
				if (wPlanszy(sasiednieR, sasiednieC))
				{
					int sasiednieJ = sasiednieR * 3 + sasiednieC;
					// sprawdzamy czy jest puste - jesli nie jest
					// to znaczy ze te 2 skoczki sie szachuja i ustawienie nie jest poprawne
					if (wszystkieUstawienia[i][sasiednieJ] != '.')
					{
						ok = false;
						break;
					}
				}
			}
			// jesli znalezlismy jakies szachujace sie skoczki to juz nie sprawdzamy
			// dalej tylko przerywamy petle i przechodzimy do kolejnego ustawienia
			if (!ok)
			{
				break;
			}
		}
		if (ok)
		{
			// jesli jest ok to przechodzimy to dodajemy do listy poprawnych ustawien
			U3.push_back(wszystkieUstawienia[i]);
		}
	}

	vector<string> U;

	for (unsigned i = 0; i < U3.size(); i++)
	{
		for (unsigned j = 0; j < U3.size(); j++)
		{
			if (!kolizjaZPoprzednimBlokiemm(1, 0, n, U3[i], U3[j]))
			{
				string u = U3[j] + U3[i];
				U.push_back(u);
			}	
		}
	}

	int poprzedniWybor = 0;

	int reszta = n % (rozmiarBloku / 3); // reszta z dzielenia liczby wierszy przez 3
	// jesli reszta ta jest wieksza jest rozna od zera to znaczy
	// ze ostatni przegladany "kwadrat" bedzie niepelny

	// wektor przechowujacy wynikowe ustawienie
	vector<string> wektorWynikowy; 

	// przechodzimy przez dane wejsciowe
	for (int i = 0; i < n * 3; i += rozmiarBloku)
	{
		int maks = -1;
		int najlepszyWybor = 0;

		// przechodzimy przez wszystkie ustawienia jakie mozemy sprawdzic
		// w obecnym bloku
		for (unsigned j = 0; j < U.size(); j++)
		{
			if (kolizjaZeZniszczonymBlokiem(daneWejsciowe, i, U[j])) continue;
			if (kolizjaZPoprzednimBlokiemm(i, reszta, n, U[j], U[poprzedniWybor])) continue;
			if (nieprawidlowyOstatniBlok(i, reszta, n, U[j])) continue;
			int ile = policz(U[j]);
			// jest ustawienie jest poprawne to sprawdzamy czy znalezlismy
			// najlepsze do tej pory
			if (ile >= maks)
			{
				maks = ile;
				najlepszyWybor = j;
			}
		}

		dodajDoWyniku(daneWejsciowe, wektorWynikowy, i, U[najlepszyWybor]);
		poprzedniWybor = najlepszyWybor;
	}

	int wynik = 0;

	// o ile musimy uciac na dole ze wzgledu na to, ze
	// dane nie dzielimy sie rowno na 3
	int obciecie = 0;
	if (reszta != 0) obciecie = (rozmiarBloku / 3) - reszta;

	for (unsigned i = 0; i < wektorWynikowy.size() - obciecie; i++)
	{
		for (int j = 0; j < 3; j++)
		{
			if (wektorWynikowy[i][j] == 'S') wynik++;
		}
	}

	printf("%d\n", wynik);
	for (unsigned i = 0; i < wektorWynikowy.size() - obciecie; i++)
	{
		printf("%s\n", wektorWynikowy[i].c_str());

	}
	return 0;
}