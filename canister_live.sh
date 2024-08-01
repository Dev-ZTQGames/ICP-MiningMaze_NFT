dfx deploy --network=ic --with-cycles 100000000000  NFT_MiningMaze --argument "(
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
