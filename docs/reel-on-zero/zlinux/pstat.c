/* pstat: attach A53 PMU counters to a running pid for T seconds; print rates.  perf_event_open, no libs. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <linux/perf_event.h>
#include <linux/hw_breakpoint.h>
struct ev { const char *name; unsigned long code; int fd; long long v; };
static struct ev evs[] = {
  {"cycles",         0x11},{"inst_retired",   0x08},{"stall_frontend", 0x23},{"stall_backend",  0x24},
  {"l1d_refill",     0x03},{"l1d_access",     0x04},{"br_mis_pred",    0x10},{"br_pred",        0x12},
  {"l1i_refill",     0x01},{"l2d_refill",     0x17},{"mem_access",     0x13},{"l1i_access",     0x14},
  /* Cortex-A53 implementation-defined stall events (TRM "Events" table, 0xE0-0xE8):
     0xE0 IQ empty, no i-cache/uTLB miss/predecode error pending   (front-end starved)
     0xE1 IQ empty, i-cache miss pending   0xE2 IQ empty, uTLB miss pending   0xE3 IQ empty, pre-decode error
     0xE4 interlock, not SIMD/FP   0xE5 interlock, load   0xE6 interlock, store
     0xE7 LSU request unavailable (structural)   0xE8 store-buffer full */
  {"iq_empty",       0xE0},{"iq_empty_icmiss", 0xE1},{"iq_empty_utlb",  0xE2},{"iq_empty_predec",0xE3},
  {"ilock_other",    0xE4},{"ilock_load",     0xE5},{"ilock_store",    0xE6},{"lsu_busy",       0xE7},{"sb_full",        0xE8}};
#define N (sizeof evs/sizeof evs[0])
static int popen_ev(unsigned long code,int pid){
  struct perf_event_attr a; memset(&a,0,sizeof a); a.type=PERF_TYPE_RAW; a.size=sizeof a; a.config=code;
  a.disabled=1; a.exclude_kernel=1; a.exclude_hv=1; a.inherit=1;
  return syscall(__NR_perf_event_open,&a,pid,-1,-1,0);}
int main(int argc,char**argv){
  if(argc<3){fprintf(stderr,"usage: pstat PID SECONDS\n");return 1;}
  int pid=atoi(argv[1]); double secs=atof(argv[2]);
  /* the A53 has 6 event counters + cycle counter: run in two rounds of <=6 events */
  for(unsigned r=0;r<N;r+=6){
    unsigned e=r+6<N?r+6:N;
    for(unsigned i=r;i<e;i++){evs[i].fd=popen_ev(evs[i].code,pid); if(evs[i].fd<0){perror(evs[i].name);}}
    for(unsigned i=r;i<e;i++) if(evs[i].fd>=0) ioctl(evs[i].fd,PERF_EVENT_IOC_RESET,0), ioctl(evs[i].fd,PERF_EVENT_IOC_ENABLE,0);
    usleep((useconds_t)(secs*1e6/4));
    for(unsigned i=r;i<e;i++) if(evs[i].fd>=0){ioctl(evs[i].fd,PERF_EVENT_IOC_DISABLE,0); if(read(evs[i].fd,&evs[i].v,8)!=8) evs[i].v=-1; close(evs[i].fd);}
  }
  double cyc=evs[0].v, ins=evs[1].v;
  printf("PSTAT pid=%d window=%.1fs (four rounds of <=6 events; cycles/inst from round 1)\n",pid,secs);
  for(unsigned i=0;i<N;i++) printf("  %-15s %14lld\n",evs[i].name,evs[i].v);
  printf("  IPC %.2f | L1D miss %.2f%% of access | br mispred %.2f%% of br | L1I refill/kinst %.2f | L1I miss %.2f%% | L2 refill/kinst %.2f\n",
    ins/cyc, 100.0*evs[4].v/evs[5].v, 100.0*evs[6].v/(evs[6].v+evs[7].v), 1000.0*evs[8].v/ins, 100.0*evs[8].v/evs[11].v, 1000.0*evs[9].v/ins);
  printf("  A53 stalls (%% of cycles): IQ-empty %.1f (icmiss %.1f utlb %.1f predec %.1f) | interlock other %.1f load %.1f store %.1f | LSU busy %.1f | SB full %.1f\n",
    100.0*evs[12].v/cyc,100.0*evs[13].v/cyc,100.0*evs[14].v/cyc,100.0*evs[15].v/cyc,100.0*evs[16].v/cyc,100.0*evs[17].v/cyc,100.0*evs[18].v/cyc,100.0*evs[19].v/cyc,100.0*evs[20].v/cyc);
  return 0;}
