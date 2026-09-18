/*
 * MIT License
 * Copyright (c) [Year] ARM-software
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 */

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include "pmuv3_plugin_bundle.h"
#include "pmuv3_processing.h"

/* CSV header from the live bundle table (event_names/num_events) — never a hardcoded copy. */
static void print_hdr(FILE *outFile, int ctx) {
    if (ctx) fprintf(outFile, "CONTEXT,");
    for (uint64_t hi = 0; hi < num_events; hi++)
        fprintf(outFile, "%s%s", event_names[hi], hi + 1 < num_events ? "," : "\n");
}

#define MAX_SIZE 10000

uint64_t *cd_arr0 = NULL;
uint64_t *cd_arr1 = NULL;
uint64_t *cd_arr2 = NULL;
uint64_t *cd_arr3 = NULL;
uint64_t *cd_arr4 = NULL;
uint64_t *cd_arr5 = NULL;
uint64_t *cd_arr6 = NULL;

uint64_t *cd_arr_e0 = NULL;
uint64_t *cd_arr_e1 = NULL;
uint64_t *cd_arr_e2 = NULL;
uint64_t *cd_arr_e3 = NULL;
uint64_t *cd_arr_e4 = NULL;
uint64_t *cd_arr_e5 = NULL;
uint64_t *cd_arr_e6 = NULL;
const char *context_arr[MAX_SIZE];

uint64_t arr_size0 = 0;
uint64_t arr_size1 = 0;
uint64_t arr_size2 = 0;
uint64_t arr_size3 = 0;
uint64_t arr_size4 = 0;
uint64_t arr_size5 = 0;
uint64_t arr_size6 = 0;
uint64_t context_count = 0;
uint64_t cycle_diff_0,cycle_diff_1,cycle_diff_2,cycle_diff_3,cycle_diff_4,cycle_diff_5,cycle_diff_6;

// Function to add an element to the end of a specific array
void pushback(uint64_t **arr, uint64_t *arr_size, uint64_t value) {
    if (*arr_size == 0) {
        *arr = (uint64_t *)malloc(sizeof(uint64_t));
    } else {
        *arr = (uint64_t *)realloc(*arr, (*arr_size + 1) * sizeof(uint64_t));
    }

    // Check if memory allocation succeeded
    if (*arr == NULL) {
        printf("Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
    // Append the value to the array
    (*arr)[(*arr_size)++] = value;
}

// Function to push a context string into the array
void push_context(const char *context) {
    if (context_count < MAX_SIZE) {
        // Allocate memory for the context string
        context_arr[context_count] = malloc(strlen(context) + 1);
        if (context_arr[context_count] == NULL) {
            fprintf(stderr, "Memory allocation failed\n");
            exit(1);
        }

        // Copy the context string into the allocated memory
        strcpy((char *)context_arr[context_count], context);

        // Increment the context count
        context_count++;
    } else {
        fprintf(stderr, "Context array is full\n");
    }
}

// Function to open the CSV file corresponding to the bundle number
FILE* open_csv_file(int bundle_num) {
    FILE* outFile = NULL;
    if (bundle_num >= 0 && bundle_num <= 14) {
        char filename[20]; // Assuming maximum filename length is 20 characters
        sprintf(filename, "bundle%d.csv", bundle_num);
        outFile = fopen(filename, "w");
        if (outFile == NULL) {
            perror("Error opening CSV file");
        }
    } else {
        fprintf(stderr, "Invalid bundle number: %d\n", bundle_num);
    }
    return outFile;
}

void process_data(int bundle_num) {
    generate_cycle_diff(num_events);
    FILE *outFile = open_csv_file(bundle_num);
    write_column_names_to_csv(bundle_num, outFile);
    write_to_csv(bundle_num, outFile);
    fclose(outFile);
    free(cd_arr0);
    free(cd_arr1);
    free(cd_arr2);
    free(cd_arr3);
    free(cd_arr4);
    free(cd_arr5);
    free(cd_arr6);
    free(cd_arr_e0);
    free(cd_arr_e1);
    free(cd_arr_e2);
    free(cd_arr_e3);
    free(cd_arr_e4);
    free(cd_arr_e5);
    free(cd_arr_e6);
}

void post_process(int bundle_num) {
    cycle_diff(num_events);
    FILE *outFile = open_csv_file(bundle_num);
    add_column_names_to_csv(bundle_num, outFile);
    add_values_to_csv(bundle_num, outFile);
    fclose(outFile);
    free(cd_arr0);
    free(cd_arr1);
    free(cd_arr2);
    free(cd_arr3);
    free(cd_arr4);
    free(cd_arr5);
    free(cd_arr6);
    free(cd_arr_e0);
    free(cd_arr_e1);
    free(cd_arr_e2);
    free(cd_arr_e3);
    free(cd_arr_e4);
    free(cd_arr_e5);
    free(cd_arr_e6);
}

void process_single_chunk(int bundle_num) {
    post_process(bundle_num);
}

void add_column_names_to_csv(int bundle_num, FILE* outFile) {
    if (bundle_num == 0) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 1) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 2) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 3) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 4) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 5) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 6) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 7) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 8) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 9) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 10) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 11) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 12) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 13) {
        print_hdr(outFile, 0);
    } else if (bundle_num == 14) {
        print_hdr(outFile, 0);
    }
}
void write_column_names_to_csv(int bundle_num, FILE *outFile) {
    if(bundle_num == 0) {
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 1) {
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 2) {
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 3) {
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 4) {
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 5) {
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 6){
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 7){
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 8){
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 9){
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 10){
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 11){
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 12){
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 13){
        print_hdr(outFile, 1);
    }
    else if(bundle_num == 14){
        print_hdr(outFile, 1);
    }
}

void add_values_to_csv(int bundle_num, FILE* outFile) {
    for (size_t i = 0; i < arr_size0; ++i) {
                {
            long *rows[7] = {(long*)cd_arr_e0, (long*)cd_arr_e1, (long*)cd_arr_e2, (long*)cd_arr_e3, (long*)cd_arr_e4, (long*)cd_arr_e5, (long*)cd_arr_e6};
            (void)bundle_num; /* values from the live bundle: num_events columns, never a hardcoded count */
            for (uint64_t k = 0; k < num_events && k < 7; k++)
                fprintf(outFile, "%ld%s", rows[k][i], k + 1 < num_events ? "," : "\n");
        }
    }
}

void write_to_csv(int bundle_num, FILE* outFile) {

    for (uint64_t i = 0; i < arr_size0; ++i) {
                {
            long *rows[7] = {(long*)cd_arr0, (long*)cd_arr1, (long*)cd_arr2, (long*)cd_arr3, (long*)cd_arr4, (long*)cd_arr5, (long*)cd_arr6};
            (void)bundle_num; /* values from the live bundle: num_events columns, never a hardcoded count */
            fprintf(outFile, "%s,", context_arr[i]);
            for (uint64_t k = 0; k < num_events && k < 7; k++)
                fprintf(outFile, "%ld%s", rows[k][i], k + 1 < num_events ? "," : "\n");
        }
    }
}

void cycle_diff(int num_events) {
    for(int k = 0; k < num_events; ++k) {
        uint64_t start = event_counts[0].start_cnt[k];
        uint64_t end = event_counts[0].end_cnt[k];
        uint64_t diff = end - start;
        printf("End is %" PRIu64 ", Start is %" PRIu64 "\n", end, start);
        if (end < start) {
            fprintf(stderr, "Counter wrapped for event index %d: end=%" PRIu64 " start=%" PRIu64 "\n",
                    k, end, start);
        }
        switch (k) {
            case 0:
                pushback(&cd_arr_e0, &arr_size0, diff);
                break;
            case 1:
                pushback(&cd_arr_e1, &arr_size1, diff);
                break;
            case 2:
                pushback(&cd_arr_e2, &arr_size2, diff);
                break;
            case 3:
                pushback(&cd_arr_e3, &arr_size3, diff);
                break;
            case 4:
                pushback(&cd_arr_e4, &arr_size4, diff);
                break;
            case 5:
                pushback(&cd_arr_e5, &arr_size5, diff);
                break;
            case 6:
                pushback(&cd_arr_e6, &arr_size6, diff);
                break;
            default:
                fprintf(stderr, "Invalid index: %d\n", k);
                break;
        }
    }
}

void generate_cycle_diff(int num_events) {
    for (uint64_t i = 0; i < global_index; ++i) {
        push_context(event_counts[i].context);
        for(int k = 0; k < num_events; ++k) {
            uint64_t start = event_counts[i].start_cnt[k];
            uint64_t end = event_counts[i].end_cnt[k];
            uint64_t diff = end - start;
	    printf("End is %" PRIu64 ", Start is %" PRIu64 ", diff is %" PRIu64 "\n",
                   end, start, diff);
            if (end < start) {
                fprintf(stderr, "Counter wrapped for context index %" PRIu64
                        " event index %d: end=%" PRIu64 " start=%" PRIu64 "\n",
                        i, k, end, start);
            }
            switch (k) {
                case 0:
                    pushback(&cd_arr0, &arr_size0, diff);
                    break;
                case 1:
                    pushback(&cd_arr1, &arr_size1, diff);
                    break;
                case 2:
                    pushback(&cd_arr2, &arr_size2, diff);
                    break;
                case 3:
                    pushback(&cd_arr3, &arr_size3, diff);
                    break;
                case 4:
                    pushback(&cd_arr4, &arr_size4, diff);
                    break;
                case 5:
                    pushback(&cd_arr5, &arr_size5, diff);
                    break;
                case 6:
                    pushback(&cd_arr6, &arr_size6, diff);
                    break;
                default:
                    fprintf(stderr, "Invalid index: %d\n", k);
                    break;
            }
        }
    }
}
