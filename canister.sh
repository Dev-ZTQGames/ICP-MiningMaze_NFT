dfx deploy --network=ic NFT_MiningMaze_test --argument "(
  principal \"p63kj-vlrqf-chucp-shmgr-gnuj7-uop7f-flj32-qzfgc-l55nf-vuutz-gae\", 
  record {
    logo = record {
      logo_type = \"image/png\";
      data = \"\";
    };
    name = \"Mining Maze\";
    symbol = \"MM\";
    maxLimit = 9554;
  }
)"
