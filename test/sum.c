
#define N	1024
volatile int t[N];

int main() {
	int s = 0;
	for(int i = 0; i < N; i++)
		s += t[i];
	return s;
}

void _exit(int rc) {
	while(1);
}
