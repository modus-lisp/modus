# Attestation documents from real enclaves

Each `.cose` is a COSE_Sign1 attestation document returned by `/dev/nsm` inside
a running Nitro enclave, pulled out over the vsock console; the `.pcrs.json`
beside it is what `kiln image nitro` computed OFFLINE for the EIF that was
running.  Re-verify any of them, no AWS account needed:

    python3 test/nitro/verify-attestation.py test/nitro/records/2026-10-07-m5.xlarge-us-east-2.cose \
        --nonce aabbccddeeff --user-data 01020304 \
        --pcr0 $(jq -r .PCR0 test/nitro/records/2026-10-07-m5.xlarge-us-east-2.pcrs.json) \
        --pcr1 $(jq -r .PCR1 ...) --pcr2 $(jq -r .PCR2 ...)

(the leaf certificate expires about three hours after issue; after that the
chain check reports the date, which is the expected outcome, not a defect.)

| date | instance | enclave | modus | kiln | verdict |
|---|---|---|---|---|---|
| 2026-10-07 01:58 UTC | m5.xlarge, us-east-2, on-demand | 2 vCPU, 3072 MB, no debug | nitro branch (NSM 0x3000 fix) | master | PASS: chain, signature, PCR0/1/2 = offline EIF PCRs, nonce, user_data |
