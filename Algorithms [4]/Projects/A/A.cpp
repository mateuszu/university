#include <cstdio>
#include <algorithm>
#include <queue>
#include <cstring>

using namespace std;



int n, m;
const int MAX_N = 2002;
const int MAX_M = 2002;
char line[MAX_N][MAX_M + 100];
bool vis[MAX_N][MAX_M];

int main(int argc, char *argv[]) {
    scanf("%d%d", &n, &m);

    int m1 = m + 1;
    int n1 = n + 1;

    fgets(line[1], m + 10, stdin);
    for (int i = 1; i <= n; ++i) { // srodek
        fgets(line[i] + 1, m + 10, stdin);
    }

    memset(vis[0], true, (size_t) m1); // granica gora

    for (int i = 0; i <= n1; ++i) { //lewo i prawo
        vis[i][0] = true;
        vis[i][m1] = true;
    }

    memset(vis[n1], true, (size_t) m1); // granica dol

    /*****************************************************************/

    int result = 0;
    for (int q = 1, i, j; q <= n; ++q) {
        for (int w = 1; w <= m; ++w) {
            if (!vis[q][w] && line[q][w] != 'A') {

                i = q; j = w;
                int i2, j2;
                char c, c2;
                vis[i][j] = true;
                queue<int> Qi, Qj;
                Qi.push(i);
                Qj.push(j);

                while (!Qi.empty()) {
                    i = Qi.front();
                    Qi.pop();
                    j = Qj.front();
                    Qj.pop();
                    c = line[i][j];

//        printf("(%d, %d, %c) -> \n", i, j, c);

                    i2 = i; j2 = j - 1; c2 = line[i2][j2]; // lewo
                    if (!vis[i2][j2] && (c2 == 'F' || c2 == 'D' || c2 == 'E') && (c == 'F' || c == 'B' || c == 'C')) {
                        Qi.push(i2);
                        Qj.push(j2);
                        vis[i2][j2] = true;
//            printf("\tlewo (%d, %d, %c) \n", i2, j2, c2);
                    }
                    j2 = j + 1; c2 = line[i2][j2]; // prawo
                    if (!vis[i2][j2] && (c == 'F' || c == 'D' || c == 'E') && (c2 == 'F' || c2 == 'B' || c2 == 'C')) {
                        Qi.push(i2);
                        Qj.push(j2);
                        vis[i2][j2] = true;
//            printf("\tprawo (%d, %d, %c) \n", i2, j2, c2);
                    }

                    j2 = j; i2 = i - 1;  c2 = line[i2][j2]; // gora
                    if (!vis[i2][j2] && (c2 == 'F' || c2 == 'B' || c2 == 'E') && (c == 'F' || c == 'C' || c == 'D')) {
                        Qi.push(i2);
                        Qj.push(j2);
                        vis[i2][j2] = true;
//            printf("\tgora (%d, %d, %c) \n", i2, j2, c2);
                    }

                    i2 = i + 1;  c2 = line[i2][j2]; // dol
                    if (!vis[i2][j2] && (c == 'F' || c == 'B' || c == 'E') && (c2 == 'F' || c2 == 'C' || c2 == 'D')) {
                        Qi.push(i2);
                        Qj.push(j2);
                        vis[i2][j2] = true;
//            printf("\tdol (%d, %d, %c) \n", i2, j2, c2);
                    }
                }


                ++result;
            }
        }
    }
    printf("%d\n", result);

    return 0;
}