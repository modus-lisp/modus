/* vpxbench: decode an IVF (VP8) file N times with libvpx, single thread, report ms/frame. */
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#define _GNU_SOURCE
#include <sched.h>
#include <vpx/vpx_decoder.h>
#include <vpx/vp8dx.h>
static double now_ms(void){struct timespec t;clock_gettime(CLOCK_MONOTONIC,&t);return t.tv_sec*1000.0+t.tv_nsec/1e6;}
static unsigned le32(const unsigned char*p){return p[0]|p[1]<<8|p[2]<<16|p[3]<<24;}
static int decode_all(const unsigned char*buf,long len,unsigned long*ysum){
  vpx_codec_ctx_t c; vpx_codec_dec_cfg_t cfg={0}; cfg.threads=1;
  if(vpx_codec_dec_init(&c,vpx_codec_vp8_dx(),&cfg,0)){fprintf(stderr,"init fail\n");exit(2);}
  long off=32;int n=0;
  while(off+12<=len){unsigned sz=le32(buf+off);const unsigned char*fr=buf+off+12;
    if(vpx_codec_decode(&c,fr,sz,NULL,0)){fprintf(stderr,"decode error frame %d: %s\n",n,vpx_codec_error_detail(&c));exit(3);}
    vpx_codec_iter_t it=NULL;vpx_image_t*img;
    while((img=vpx_codec_get_frame(&c,&it))){ if(ysum){for(int y=0;y<img->d_h;y++)for(int x=0;x<img->d_w;x++)*ysum+=img->planes[0][y*img->stride[0]+x];} }
    off+=12+sz;n++;}
  vpx_codec_destroy(&c);return n;}
int main(int argc,char**argv){
  if(getenv("VPX_CPU")){cpu_set_t s;CPU_ZERO(&s);CPU_SET(atoi(getenv("VPX_CPU")),&s);if(sched_setaffinity(0,sizeof s,&s))perror("setaffinity");}

  for(int a=1;a<argc;a++){FILE*f=fopen(argv[a],"rb");if(!f){perror(argv[a]);return 1;}
    fseek(f,0,SEEK_END);long len=ftell(f);fseek(f,0,SEEK_SET);unsigned char*buf=malloc(len);fread(buf,1,len,f);fclose(f);
    unsigned long ysum=0;int n=decode_all(buf,len,&ysum);
    double t0=now_ms();int P=getenv("VPX_PASSES")?atoi(getenv("VPX_PASSES")):5;for(int k=0;k<P;k++)n=decode_all(buf,len,NULL);double ms=now_ms()-t0;
    printf("VPXBENCH %s: %d frames x%d in %.0f ms => %.2f ms/frame (%.1f fps) [libvpx %s, threads=1, ysum=%lu]\n",argv[a],n,P,ms,ms/((double)P*n),(double)P*n*1000/ms,vpx_codec_version_str(),ysum);free(buf);}
  return 0;}
