dfx canister --playground call dip721_nft_container mintDip721 \
"(
  principal\"p63kj-vlrqf-chucp-shmgr-gnuj7-uop7f-flj32-qzfgc-l55nf-vuutz-gae\", 
  vec { 
    record {
      purpose = variant{Rendered};
      data = blob\"hello\";
      key_val_data = vec {
        record { key = \"description\"; val = variant{TextContent=\"The NFT metadata can hold arbitrary metadata\"}; };
        record { key = \"tag\"; val = variant{TextContent=\"anime\"}; };
        record { key = \"contentType\"; val = variant{TextContent=\"text/plain\"}; };
        record { key = \"locationType\"; val = variant{Nat8Content=4:nat8} };
      }
    }
  }
)"
