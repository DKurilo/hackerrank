#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#define MAX_CHARACTERS 1005
#define MAX_PARAGRAPHS 5

struct word {
    char* data;
};

struct sentence {
    struct word* data;
    int word_count;//denotes number of words in a sentence
};

struct paragraph {
    struct sentence* data  ;
    int sentence_count;//denotes number of sentences in a paragraph
};

struct document {
    struct paragraph* data;
    int paragraph_count;//denotes number of paragraphs in a document
};

int is_2pown(int m) {
    if (m <= 0) {
        return 0;
    }
    while (m > 1) {
        if (m % 2 == 1) {
            return 0;
        }
        m /= 2;
    }
    return 1;
}

char* create_bufer(int length) {
    char *buf = malloc(sizeof(*buf) * length);
    for (int i = 0; i < length; i++) {
        buf[i] = '\0';
    }
    return buf;
}

struct document get_document(char* text) {
    int max_len = strlen(text);
    struct document d;
    int wc = 0, sc = 0, pc = 0, pos = 0;
    char *buf;
    for (int i = 0; i <= max_len; i++) {
        char c = text[i];
        if (i == max_len || c == '\n' || c == '.' || c == ' ') {
            if (pos > 0) {
                buf = realloc(buf, sizeof(char) * (pos + 1));
                d.data[pc].data[sc].data[wc].data = buf;
                pos = 0;
                wc++;
            }
            if (wc > 0 && (i == max_len || c == '\n' || c == '.')) {
                d.data[pc].data[sc].data = realloc(
                    d.data[pc].data[sc].data,
                    sizeof(struct word) * wc
                );
                d.data[pc].data[sc].word_count = wc;
                wc = 0;
                sc++;
            }
            if (sc > 0 && (i == max_len || c == '\n')) {
                d.data[pc].data = realloc(
                    d.data[pc].data,
                    sizeof(struct sentence) * sc
                );
                d.data[pc].sentence_count = sc;
                sc = 0;
                pc++;
            }
        } else {
            if (pos == 0) {
                if (wc == 0) {
                    if (sc == 0) {
                        if (pc == 0) {
                            d.data = malloc(sizeof(struct paragraph));
                        } else if (is_2pown(pc) == 1) {
                            d.data = realloc(
                                d.data,
                                sizeof(struct paragraph) * pc * 2
                            );
                        }
                        d.data[pc].data = malloc(sizeof(struct sentence));
                    } else if (is_2pown(sc) == 1) {
                        d.data[pc].data = realloc(
                            d.data[pc].data,
                            sizeof(struct sentence) * sc * 2
                        );
                    }
                    d.data[pc].data[sc].data = malloc(sizeof(struct word));
                } else if (is_2pown(wc) == 1) {
                    d.data[pc].data[sc].data = realloc(
                        d.data[pc].data[sc].data,
                        sizeof(struct word) * wc * 2
                    );
                }
                buf = create_bufer(max_len);
            }
            buf[pos] = c;
            pos++;
        }
    }
    d.data = realloc(
        d.data,
        sizeof(struct paragraph) * pc
    );
    d.paragraph_count = pc;
    return d;
}

struct word kth_word_in_mth_sentence_of_nth_paragraph(struct document Doc, int k, int m, int n) {
    return Doc.data[n - 1].data[m - 1].data[k - 1];
}

struct sentence kth_sentence_in_mth_paragraph(struct document Doc, int k, int m) { 
    return Doc.data[m - 1].data[k - 1];
}

struct paragraph kth_paragraph(struct document Doc, int k) {
    return Doc.data[k - 1];
}


void print_word(struct word w) {
    printf("%s", w.data);
}

void print_sentence(struct sentence sen) {
    for(int i = 0; i < sen.word_count; i++) {
        print_word(sen.data[i]);
        if (i != sen.word_count - 1) {
            printf(" ");
        }
    }
}

void print_paragraph(struct paragraph para) {
    for(int i = 0; i < para.sentence_count; i++){
        print_sentence(para.data[i]);
        printf(".");
    }
}

void print_document(struct document doc) {
    for(int i = 0; i < doc.paragraph_count; i++) {
        print_paragraph(doc.data[i]);
        if (i != doc.paragraph_count - 1)
            printf("\n");
    }
}

char* get_input_text() {	
    int paragraph_count;
    scanf("%d", &paragraph_count);

    char p[MAX_PARAGRAPHS][MAX_CHARACTERS], doc[MAX_CHARACTERS];
    memset(doc, 0, sizeof(doc));
    getchar();
    for (int i = 0; i < paragraph_count; i++) {
        scanf("%[^\n]%*c", p[i]);
        strcat(doc, p[i]);
        if (i != paragraph_count - 1)
            strcat(doc, "\n");
    }

    char* returnDoc = (char*)malloc((strlen (doc)+1) * (sizeof(char)));
    strcpy(returnDoc, doc);
    return returnDoc;
}

int main() 
{
    char* text = get_input_text();
    struct document Doc = get_document(text);

    int q;
    scanf("%d", &q);

    while (q--) {
        int type;
        scanf("%d", &type);

        if (type == 3){
            int k, m, n;
            scanf("%d %d %d", &k, &m, &n);
            struct word w = kth_word_in_mth_sentence_of_nth_paragraph(Doc, k, m, n);
            print_word(w);
        }

        else if (type == 2) {
            int k, m;
            scanf("%d %d", &k, &m);
            struct sentence sen= kth_sentence_in_mth_paragraph(Doc, k, m);
            print_sentence(sen);
        }

        else{
            int k;
            scanf("%d", &k);
            struct paragraph para = kth_paragraph(Doc, k);
            print_paragraph(para);
        }
        printf("\n");
    }     
}
