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

/*
 * Performance Monitoring accessing PMUV3 counters
 * Author: Gayathri  Narayana Yegna Narayanan (gayathrinarayana.yegnanarayanan@arm.com)
 * Description: This plugin initializes performance monitoring for specific hardware events grouped into 15 bundles, reads cycle counts and cleans up the resources after that. 
 */

#include "pmuv3_plugin_bundle.h"

int tests_failed;
int tests_verbose;
struct PerfData *perf_data;
struct CountData count_data;
struct perf_thread_map *global_threads;
int num_bundles;
uint64_t num_events; 
int *event_values = NULL;
char **event_names=NULL;

uint64_t start_0,start_1,start_2,start_3,start_4,start_5,start_6,start_7;
uint64_t end_0,end_1,end_2,end_3,end_4,end_5,end_6,end_7;
static int debug_direct_printed[MAX_EVENTS];
static int debug_fallback_printed[MAX_EVENTS];

/* Cortex-A53 (Pi Zero 2 W) bundle set: only events in PMCEID0 (0x67FFBFFF, no PMCEID1 events, no INST_SPEC/STALL_*)
   plus the A53 implementation-defined stall events 0xE0-0xE8 (Cortex-A53 TRM).  Max 6 events per bundle.  Generated. */
bundles bundle0[] = {
    {"CPU_CYCLES", 0x11},
    {"INST_RETIRED", 0x08},
    {"LD_RETIRED", 0x06},
    {"ST_RETIRED", 0x07},
    {"PC_WRITE_RETIRED", 0x0C},
    {"BR_IMMED_RETIRED", 0x0D},
};
bundles bundle1[] = {
    {"CPU_CYCLES", 0x11},
    {"BR_MIS_PRED", 0x10},
    {"BR_PRED", 0x12},
    {"UNALIGNED_LDST_RETIRED", 0x0F},
    {"EXC_TAKEN", 0x09},
    {"EXC_RETURN", 0x0A},
};
bundles bundle2[] = {
    {"CPU_CYCLES", 0x11},
    {"L1I_CACHE_REFILL", 0x01},
    {"L1I_CACHE", 0x14},
    {"L1I_TLB_REFILL", 0x02},
    {"INST_RETIRED", 0x08},
};
bundles bundle3[] = {
    {"CPU_CYCLES", 0x11},
    {"L1D_CACHE_REFILL", 0x03},
    {"L1D_CACHE", 0x04},
    {"L1D_CACHE_WB", 0x15},
    {"L1D_TLB_REFILL", 0x05},
    {"MEM_ACCESS", 0x13},
};
bundles bundle4[] = {
    {"CPU_CYCLES", 0x11},
    {"L2D_CACHE", 0x16},
    {"L2D_CACHE_REFILL", 0x17},
    {"L2D_CACHE_WB", 0x18},
    {"BUS_ACCESS", 0x19},
    {"BUS_CYCLES", 0x1D},
};
bundles bundle5[] = {
    {"CPU_CYCLES", 0x11},
    {"A53_IQ_EMPTY", 0xE0},
    {"A53_IQ_EMPTY_ICMISS", 0xE1},
    {"A53_IQ_EMPTY_UTLB", 0xE2},
    {"A53_IQ_EMPTY_PREDEC", 0xE3},
};
bundles bundle6[] = {
    {"CPU_CYCLES", 0x11},
    {"A53_ILOCK_OTHER", 0xE4},
    {"A53_ILOCK_LOAD", 0xE5},
    {"A53_ILOCK_STORE", 0xE6},
    {"A53_LSU_BUSY", 0xE7},
    {"A53_SB_FULL", 0xE8},
};
bundles bundle7[] = {
    {"CPU_CYCLES", 0x11},
    {"INST_RETIRED", 0x08},
    {"L1I_CACHE_REFILL", 0x01},
    {"L1D_CACHE_REFILL", 0x03},
    {"BR_MIS_PRED", 0x10},
    {"L2D_CACHE_REFILL", 0x17},
};
bundles bundle8[] = {
    {"CPU_CYCLES", 0x11},
    {"INST_RETIRED", 0x08},
    {"A53_IQ_EMPTY_ICMISS", 0xE1},
    {"A53_ILOCK_LOAD", 0xE5},
    {"A53_ILOCK_OTHER", 0xE4},
    {"A53_LSU_BUSY", 0xE7},
};
bundles bundle9[] = {
    {"CPU_CYCLES", 0x11},
    {"MEM_ACCESS", 0x13},
    {"BUS_ACCESS", 0x19},
    {"MEMORY_ERROR", 0x1A},
    {"CID_WRITE_RETIRED", 0x0B},
    {"TTBR_WRITE_RETIRED", 0x1C},
};
bundles bundle10[] = {
    {"CPU_CYCLES", 0x11},
    {"INST_RETIRED", 0x08},
    {"L1I_CACHE_REFILL", 0x01},
    {"L1D_CACHE_REFILL", 0x03},
    {"BR_MIS_PRED", 0x10},
    {"L2D_CACHE_REFILL", 0x17},
};
bundles bundle11[] = {
    {"CPU_CYCLES", 0x11},
    {"INST_RETIRED", 0x08},
    {"L1I_CACHE_REFILL", 0x01},
    {"L1D_CACHE_REFILL", 0x03},
    {"BR_MIS_PRED", 0x10},
    {"L2D_CACHE_REFILL", 0x17},
};
bundles bundle12[] = {
    {"CPU_CYCLES", 0x11},
    {"INST_RETIRED", 0x08},
    {"L1I_CACHE_REFILL", 0x01},
    {"L1D_CACHE_REFILL", 0x03},
    {"BR_MIS_PRED", 0x10},
    {"L2D_CACHE_REFILL", 0x17},
};
bundles bundle13[] = {
    {"CPU_CYCLES", 0x11},
    {"INST_RETIRED", 0x08},
    {"L1I_CACHE_REFILL", 0x01},
    {"L1D_CACHE_REFILL", 0x03},
    {"BR_MIS_PRED", 0x10},
    {"L2D_CACHE_REFILL", 0x17},
};
bundles bundle14[] = {
    {"CPU_CYCLES", 0x11},
    {"INST_RETIRED", 0x08},
    {"L1I_CACHE_REFILL", 0x01},
    {"L1D_CACHE_REFILL", 0x03},
    {"BR_MIS_PRED", 0x10},
    {"L2D_CACHE_REFILL", 0x17},
};
struct PMUv3_Bundle_Data event_counts[10000];
uint64_t global_index = 0;
uint64_t get_next_index(void) {
    return global_index++;
}

static int pmuv3_read_direct(struct perf_event_mmap_page *pc, uint64_t *value)
{
#if !defined(__aarch64__)
    (void)pc;
    (void)value;
    return -1;
#else
    if (pc == NULL || pc->cap_user_rdpmc == 0)
        return -1;

    uint32_t seq;
    uint32_t index;
    uint64_t offset;
    uint16_t width;
    uint64_t reg_value = 0;

    do {
        seq = pc->lock;
        __atomic_thread_fence(__ATOMIC_ACQUIRE);

        index = pc->index;
        offset = pc->offset;
        width = pc->pmc_width;

        if (index == 0)
            return -1;

        uint32_t pmc_index = index - 1;
        if (pmc_index == 31) {
            asm volatile("mrs %0, pmccntr_el0" : "=r"(reg_value));
        } else {
            asm volatile("msr pmselr_el0, %0" : : "r"((uint64_t)pmc_index));
            asm volatile("isb");
            asm volatile("mrs %0, pmxevcntr_el0" : "=r"(reg_value));
        }

        __atomic_thread_fence(__ATOMIC_ACQUIRE);
    } while (pc->lock != seq);

    int64_t signed_count = (int64_t)reg_value;
    if (width > 0 && width < 64) {
        signed_count <<= (64 - width);
        signed_count >>= (64 - width);
    }

    *value = (uint64_t)((int64_t)offset + signed_count);
    return 0;
#endif
}

static int pmuv3_read_event(uint64_t event_index, struct perf_counts_values *count)
{
    uint64_t value;
    const char *debug = getenv("PMUV3_DEBUG_READ_PATH");

    if (perf_data == NULL || count == NULL || event_index >= num_events)
        return -1;

    if (pmuv3_read_direct(perf_data->pc[event_index], &value) == 0) {
        if (debug != NULL && debug[0] != '\0' && debug_direct_printed[event_index] == 0) {
            fprintf(stderr, "PMUv3: event %" PRIu64 " using direct EL0 PMU read path\n",
                    event_index);
            debug_direct_printed[event_index] = 1;
        }
        count->val = value;
        return 0;
    }

    if (debug != NULL && debug[0] != '\0' && debug_fallback_printed[event_index] == 0) {
        fprintf(stderr, "PMUv3: event %" PRIu64 " using perf_evsel__read fallback path\n",
                event_index);
        debug_fallback_printed[event_index] = 1;
    }
    return perf_evsel__read(perf_data->global_evsel[event_index], 0, 0, count);
}

#if 1
int custom_print(enum libperf_print_level level,
        const char *fmt, va_list ap)
{
    //return 0;
    (void)level;
    return vfprintf(stderr, fmt, ap);
}
#endif

int pmu_counter_read(int events[]) {
    //struct perf_counts_values counts[MAX_EVENTS] = {0}; // Array to store counts for each event
    struct perf_counts_values counts[MAX_EVENTS] = {0};
    struct perf_thread_map *threads;
    struct perf_evsel *evsels[MAX_EVENTS];
    struct perf_event_mmap_page *pcs[MAX_EVENTS];
    int errs[MAX_EVENTS];

    perf_data = (struct PerfData *)malloc(sizeof(struct PerfData));


    // Initialize thread map
    threads = perf_thread_map__new_dummy();
    if (!threads) {
        // Handle error
        return -1;
    }

    perf_thread_map__set_pid(threads, 0, 0);
    global_threads = threads;

    // Loop through events and initialize attributes, evsels, and counts
    for (uint64_t i = 0; i < num_events; ++i) {
        struct perf_event_attr attr = {
            .type       = PERF_TYPE_RAW,
            .config     = events[i],
            .config1    = 0x2 // Request user access
        };

        evsels[i] = perf_evsel__new(&attr);
        if (!evsels[i]) {
            // Handle error
            return -1;
        }

        errs[i] = perf_evsel__open(evsels[i], NULL, threads);
        if (errs[i]) {
            // Handle error
            return -1;
        }

        errs[i] = perf_evsel__mmap(evsels[i], 0);
        if (errs[i]) {
            // Handle error
            return -1;
        }

        pcs[i] = perf_evsel__mmap_base(evsels[i], 0, 0);
        if (!pcs[i]) {
            // Handle error
            return -1;
        }

        perf_data->global_evsel[i] = evsels[i];
        perf_data->pc[i] = pcs[i];
        count_data.global_count[i] = counts[i];
        count_data.global_count[i].val = counts[i].val;
    }

    return 0;
}


// INIT FUNCTION
int init_api(int argc, char **argv, int event_vals[]) {
    __T_START;
    libperf_init(custom_print);
    if (pmu_counter_read(event_vals) != 0) {
        // Handle error
        return -1;
    }
    __T_END;
    return tests_failed == 0 ? 0 : -1;
}

//Instrumentation without local variable

// START CYCLE
uint64_t process_start_count(struct CountData *count_data) {
    if (perf_data != NULL && count_data != NULL) {
        // Check if perf_data->global_evsel_0 is not NULL
        if (perf_data->global_evsel[0] != NULL) {
            // Accessing perf_data->global_evsel_0 is safe
            pmuv3_read_event(0, &count_data->global_count[0]);
        } else {
            // Handle the case where perf_data->global_evsel_0 is NULL
            // This might indicate an error in your program
            // You can print an error message or take appropriate action
            printf("Error: perf_data->global_evsel_0 is NULL\n");
        }
    }
    for(uint64_t i =0; i < num_events; i++) {
        pmuv3_read_event(i, &count_data->global_count[i]);
        event_counts[0].start_cnt[i] = count_data->global_count[i].val;
    }
    return 0;
}

// START CYCLE
uint64_t get_start_count(struct CountData *count_data, const char* context, uint64_t index) { 
    if (perf_data != NULL && count_data != NULL) {
        // Check if perf_data->global_evsel_0 is not NULL
        if (perf_data->global_evsel[0] != NULL) {
            // Accessing perf_data->global_evsel_0 is safe
            pmuv3_read_event(0, &count_data->global_count[0]);
        } else {
            // Handle the case where perf_data->global_evsel_0 is NULL
            // This might indicate an error in your program
            // You can print an error message or take appropriate action
            printf("Error: perf_data->global_evsel_0 is NULL\n");
        }
    }
    event_counts[index].context = context;
    for(uint64_t i =0; i < num_events; i++) {
        pmuv3_read_event(i, &count_data->global_count[i]);
        event_counts[index].start_cnt[i] = count_data->global_count[i].val;
    }
    return 0;
}

//Instrumenting w/o local variable
// END CYCLE 
uint64_t process_end_count(struct CountData *count_data) {
    // Perform perf_evsel__read operation to get end count for the event at the given index
    for(uint64_t i =0; i < num_events; i++) {
        pmuv3_read_event(i, &count_data->global_count[i]);
        event_counts[0].end_cnt[i] = count_data->global_count[i].val;
    }
    return 0;
}

//Instrumenting with local variable in multiple functions.
uint64_t get_end_count(struct CountData *count_data, const char* context, uint64_t index) {

    for(uint64_t i =0; i < num_events; i++) {
        pmuv3_read_event(i, &count_data->global_count[i]);
        event_counts[index].end_cnt[i] = count_data->global_count[i].val;
    }
    //array_index--;
    event_counts[index].context = context;
    //end_index++;
    return 0;
}

// SHUTDOWN API
int shutdown_resources() {
    //printf("Entering shutdown_resources\n");
    if (perf_data == NULL) {
        printf("perf_data is NULL\n");
        return -1; // Return an error code if perf_data is NULL
    }
    // Debugging statements for global_evsel_0
    for(uint64_t i =0; i < num_events; i++){
        if (perf_data->global_evsel[i] != NULL) {
            perf_evsel__munmap(perf_data->global_evsel[i]);
            perf_evsel__close(perf_data->global_evsel[i]);
            perf_evsel__delete(perf_data->global_evsel[i]);
            perf_data->global_evsel[i] = NULL;
            perf_data->pc[i] = NULL;
        }
    }
    if (global_threads != NULL) {
        perf_thread_map__put(global_threads);
        global_threads = NULL;
    }
    free_bundle_memory();
    return 0;
}
// Function to initialize a bundle
void init_bundle(bundles* bundle) {
    event_values = malloc(num_events * sizeof(int));
    event_names = (char**)malloc(num_events * sizeof(char*));

    for (size_t i = 0; i < num_events; i++) {
        event_values[i] = bundle[i].event_value;
        event_names[i] = (char*)malloc((strlen(bundle[i].name) + 1) * sizeof(char));
        strcpy(event_names[i], bundle[i].name);
    }
}

// Function to free allocated memory
void free_bundle_memory() {
    if (event_names != NULL) {
        for (size_t i = 0; i < num_events; i++) {
            free(event_names[i]);
        }
        free(event_names);
        event_names = NULL;
    }
    if (event_values != NULL) {
        free(event_values);
        event_values = NULL;
    }
}

int pmuv3_bundle_init(int num_bundles) {
    if (num_bundles < 0 || num_bundles >= TOTAL_BUNDLE_NUM) {
        printf("Error: Invalid bundle number %d\n", num_bundles);
        exit(1);
    }

    switch (num_bundles) {
        case 0: num_events = sizeof(bundle0) / sizeof(bundle0[0]); init_bundle(bundle0); break;
        case 1: num_events = sizeof(bundle1) / sizeof(bundle1[0]);init_bundle(bundle1); break;
        case 2: num_events = sizeof(bundle2) / sizeof(bundle2[0]);init_bundle(bundle2); break;
        case 3: num_events = sizeof(bundle3) / sizeof(bundle3[0]);init_bundle(bundle3); break;
        case 4: num_events = sizeof(bundle4) / sizeof(bundle4[0]);init_bundle(bundle4); break;
        case 5: num_events = sizeof(bundle5) / sizeof(bundle5[0]);init_bundle(bundle5); break;
        case 6: num_events = sizeof(bundle6) / sizeof(bundle6[0]);init_bundle(bundle6); break;
        case 7: num_events = sizeof(bundle7) / sizeof(bundle7[0]);init_bundle(bundle7); break;
        case 8: num_events = sizeof(bundle8) / sizeof(bundle8[0]);init_bundle(bundle8); break;
        case 9: num_events = sizeof(bundle9) / sizeof(bundle9[0]);init_bundle(bundle9); break;
        case 10: num_events = sizeof(bundle10) / sizeof(bundle10[0]);init_bundle(bundle10); break;
        case 11: num_events = sizeof(bundle11) / sizeof(bundle11[0]);init_bundle(bundle11); break;
        case 12: num_events = sizeof(bundle12) / sizeof(bundle12[0]);init_bundle(bundle12); break;
        case 13: num_events = sizeof(bundle13) / sizeof(bundle13[0]);init_bundle(bundle13); break;
        case 14: num_events = sizeof(bundle14) / sizeof(bundle14[0]);init_bundle(bundle14); break;
        default:
            printf("Argument should be one of these in the interval [0,14] \n");
            exit(1);
    }

    __T("init api", !init_api(0, NULL, event_values));
    return 0;
}
