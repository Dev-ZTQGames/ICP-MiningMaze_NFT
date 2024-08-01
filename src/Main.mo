import Nat "mo:base/Nat";
import Nat8 "mo:base/Nat8";
import Nat16 "mo:base/Nat16";
import Nat32 "mo:base/Nat32";
import Nat64 "mo:base/Nat64";
import List "mo:base/List";
import Array "mo:base/Array";
import Option "mo:base/Option";
import Bool "mo:base/Bool";
import Principal "mo:base/Principal";
import Types "./Types";

shared actor class Dip721NFT(custodian: Principal, init : Types.Dip721NonFungibleToken) = Self {
  stable var transactionId: Types.TransactionId = 0;
  stable var nfts = List.nil<Types.Nft>();
  stable var nfts_Limit : Nat64 = 5555;				// 0~5554
  stable var nftsRare = List.nil<Types.Nft>();
  stable var nftsRare_Limit : Nat64 = 2222;			// 5555~7776
  stable var nftsEpic = List.nil<Types.Nft>();
  stable var nftsEpic_Limit : Nat64 = 1111;			// 7777~8887
  stable var nftsUnique = List.nil<Types.Nft>();
  stable var nftsUnique_Limit : Nat64 = 555;			// 8888~9442
  stable var nftsLegendary = List.nil<Types.Nft>();
  stable var nftsLegendary_Limit : Nat64 = 111;		// 9443~9553
  stable var custodians = List.make<Principal>(custodian);
  stable var logo : Types.LogoResult = init.logo;
  stable var name : Text = init.name;
  stable var symbol : Text = init.symbol;
  stable var maxLimit : Nat16 = init.maxLimit;

  stable var baseURL : Text = "https://cdn.aurorahunt.xyz/nft/mm/";

  // https://forum.dfinity.org/t/is-there-any-address-0-equivalent-at-dfinity-motoko/5445/3
  let null_address : Principal = Principal.fromText("aaaaa-aa");

  public query func balanceOfDip721(user: Principal) : async Nat64 {
    return Nat64.fromNat(
      List.size(List.filter(nfts, func(token: Types.Nft) : Bool { token.owner == user })) + 
      List.size(List.filter(nftsRare, func(token: Types.Nft) : Bool { token.owner == user })) + 
      List.size(List.filter(nftsEpic, func(token: Types.Nft) : Bool { token.owner == user })) + 
      List.size(List.filter(nftsUnique, func(token: Types.Nft) : Bool { token.owner == user })) + 
      List.size(List.filter(nftsLegendary, func(token: Types.Nft) : Bool { token.owner == user }))
    );
  };

  public query func ownerOfDip721(token_id: Types.TokenId) : async Types.OwnerResult {

    if ( token_id < nfts_Limit ) {
      let item_Normal = List.find(nfts, func(token: Types.Nft) : Bool { token.id == token_id });
      switch (item_Normal) {
        case (null) {
	  return #Err(#InvalidTokenId);
        };
        case (?token) {
	  return #Ok(token.owner);
        };
      };
    };

    if ( token_id < nftsRare_Limit + nfts_Limit ) {
      let item_Rare	= List.find(nftsRare, func(token: Types.Nft) : Bool { token.id == token_id - nfts_Limit });
      switch (item_Rare) {
        case (null) {
          return #Err(#InvalidTokenId);
        };
        case (?token) {
          return #Ok(token.owner);
        };
      };
    };

    if ( token_id < nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) {
      let item_Epic	= List.find(nftsEpic, func(token: Types.Nft) : Bool { token.id == token_id - ( nftsRare_Limit + nfts_Limit ) });
      switch (item_Epic) {
        case (null) {
          return #Err(#InvalidTokenId);
        };
        case (?token) {
          return #Ok(token.owner);
        };
      };
    };

    if ( token_id < nftsUnique_Limit + nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) {
      let item_Unique = List.find(nftsUnique, func(token: Types.Nft) : Bool { token.id == token_id - ( nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) });
      switch (item_Unique) {
        case (null) {
          return #Err(#InvalidTokenId);
        };
        case (?token) {
          return #Ok(token.owner);
        };
      };
    };

    if ( token_id < nftsLegendary_Limit + nftsUnique_Limit + nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) {
      let item_Legendary = List.find(nftsLegendary, func(token: Types.Nft) : Bool { token.id == token_id - ( nftsUnique_Limit + nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) });
      switch (item_Legendary) {
        case (null) {
          return #Err(#InvalidTokenId);
        };
        case (?token) {
          return #Ok(token.owner);
        };
      };
    };

    return #Err(#InvalidTokenId);
  };

  public shared({ caller }) func safeTransferFromDip721(from: Principal, to: Principal, token_id: Types.TokenId) : async Types.TxReceipt {  
    if (to == null_address) {
      return #Err(#ZeroAddress);
    } else {
      return transferFrom(from, to, token_id, caller);
    };
  };

  public shared({ caller }) func transferFromDip721(from: Principal, to: Principal, token_id: Types.TokenId) : async Types.TxReceipt {
    return transferFrom(from, to, token_id, caller);
  };

  func transferFrom(from: Principal, to: Principal, token_id: Types.TokenId, caller: Principal) : Types.TxReceipt {

    if ( token_id < nfts_Limit ) {
	    let item = List.find(nfts, func(token: Types.Nft) : Bool { token.id == token_id });

	    switch (item) {
	      case null {
		return #Err(#InvalidTokenId);
	      };
	      case (?token) {
		if ( caller != token.owner and not List.some(custodians, func (custodian : Principal) : Bool { custodian == caller }) ) {
		  return #Err(#Unauthorized);
		} else if (Principal.notEqual(from, token.owner)) {
		  return #Err(#Other);
		} else {

		  nfts := List.map(nfts, func (item : Types.Nft) : Types.Nft {
		    if (item.id == token.id) {
		      let update : Types.Nft = {
			owner = to;
			id = item.id;
			metadata = token.metadata;
		      };
		      return update;
		    } else {
		      return item;
		    };
		  });

		  transactionId += 1;
		  return #Ok(transactionId);   

		};
	      };
	    };
    };

    if ( token_id < nftsRare_Limit + nfts_Limit ) {
	    let item = List.find(nftsRare, func(token: Types.Nft) : Bool { token.id == token_id - nfts_Limit});

	    switch (item) {
	      case null {
		return #Err(#InvalidTokenId);
	      };
	      case (?token) {
		if ( caller != token.owner and not List.some(custodians, func (custodian : Principal) : Bool { custodian == caller }) ) {
		  return #Err(#Unauthorized);
		} else if (Principal.notEqual(from, token.owner)) {
		  return #Err(#Other);
		} else {

		  nftsRare := List.map(nftsRare, func (item : Types.Nft) : Types.Nft {
		    if (item.id == token.id) {
		      let update : Types.Nft = {
			owner = to;
			id = item.id;
			metadata = token.metadata;
		      };
		      return update;
		    } else {
		      return item;
		    };
		  });

		  transactionId += 1;
		  return #Ok(transactionId);   

		};
	      };
	    };
    };

    if ( token_id < nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) {	    
	    let item = List.find(nftsEpic, func(token: Types.Nft) : Bool { token.id == token_id - ( nftsRare_Limit + nfts_Limit ) });

	    switch (item) {
	      case null {
		return #Err(#InvalidTokenId);
	      };
	      case (?token) {
		if ( caller != token.owner and not List.some(custodians, func (custodian : Principal) : Bool { custodian == caller }) ) {
		  return #Err(#Unauthorized);
		} else if (Principal.notEqual(from, token.owner)) {
		  return #Err(#Other);
		} else {

		  nftsEpic := List.map(nftsEpic, func (item : Types.Nft) : Types.Nft {
		    if (item.id == token.id) {
		      let update : Types.Nft = {
			owner = to;
			id = item.id;
			metadata = token.metadata;
		      };
		      return update;
		    } else {
		      return item;
		    };
		  });

		  transactionId += 1;
		  return #Ok(transactionId);   

		};
	      };
	    };
    };

    if ( token_id < nftsUnique_Limit + nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) {
	    let item = List.find(nftsUnique, func(token: Types.Nft) : Bool { token.id == token_id - ( nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) });

	    switch (item) {
	      case null {
		return #Err(#InvalidTokenId);
	      };
	      case (?token) {
		if ( caller != token.owner and not List.some(custodians, func (custodian : Principal) : Bool { custodian == caller }) ) {
		  return #Err(#Unauthorized);
		} else if (Principal.notEqual(from, token.owner)) {
		  return #Err(#Other);
		} else {

		  nftsUnique := List.map(nftsUnique, func (item : Types.Nft) : Types.Nft {
		    if (item.id == token.id) {
		      let update : Types.Nft = {
			owner = to;
			id = item.id;
			metadata = token.metadata;
		      };
		      return update;
		    } else {
		      return item;
		    };
		  });

		  transactionId += 1;
		  return #Ok(transactionId);   

		};
	      };
	    };
    };

    if ( token_id < nftsLegendary_Limit + nftsUnique_Limit + nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) {
	    let item = List.find(nftsLegendary, func(token: Types.Nft) : Bool { token.id == token_id - ( nftsUnique_Limit + nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) });

	    switch (item) {
	      case null {
		return #Err(#InvalidTokenId);
	      };
	      case (?token) {
		if ( caller != token.owner and not List.some(custodians, func (custodian : Principal) : Bool { custodian == caller }) ) {
		  return #Err(#Unauthorized);
		} else if (Principal.notEqual(from, token.owner)) {
		  return #Err(#Other);
		} else {

		  nftsLegendary := List.map(nftsLegendary, func (item : Types.Nft) : Types.Nft {
		    if (item.id == token.id) {
		      let update : Types.Nft = {
			owner = to;
			id = item.id;
			metadata = token.metadata;
		      };
		      return update;
		    } else {
		      return item;
		    };
		  });

		  transactionId += 1;
		  return #Ok(transactionId);   

		};
	      };
	    };
    };

    return #Err(#InvalidTokenId);
  };

  public query func supportedInterfacesDip721() : async [Types.InterfaceId] {
    return [#TransferNotification, #Burn, #Mint];
  };

  public query func logoDip721() : async Types.LogoResult {
    return logo;
  };

  public query func nameDip721() : async Text {
    return name;
  };

  public query func symbolDip721() : async Text {
    return symbol;
  };

  public query func baseURLCustom() : async Text {
    return baseURL;
  };

  public query func totalSupplyDip721() : async Nat64 {
    return Nat64.fromNat(
      List.size(nfts) + List.size(nftsRare) + List.size(nftsEpic) + List.size(nftsUnique) + List.size(nftsLegendary)
    );
  };

  public query func totalSupplyNormalCustom() : async Nat64 {
    return Nat64.fromNat(
      List.size(nfts)
    );
  };

  public query func totalSupplyRareCustom() : async Nat64 {
    return Nat64.fromNat(
      List.size(nftsRare)
    );
  };

  public query func totalSupplyEpicCustom() : async Nat64 {
    return Nat64.fromNat(
      List.size(nftsEpic)
    );
  };

  public query func totalSupplyUniqueCustom() : async Nat64 {
    return Nat64.fromNat(
      List.size(nftsUnique)
    );
  };

  public query func totalSupplyLegendaryCustom() : async Nat64 {
    return Nat64.fromNat(
      List.size(nftsLegendary)
    );
  };

  public query func getMetadataDip721(token_id: Types.TokenId) : async Types.MetadataResult {

    if ( token_id < nfts_Limit ) {
      let item_Normal = List.find(nfts, func(token: Types.Nft) : Bool { token.id == token_id });
      switch (item_Normal) {
        case (null) {
          return #Err(#InvalidTokenId);
        };
        case (?token) {
          return #Ok(token.metadata);
        };
      };
    };

    if ( token_id < nftsRare_Limit + nfts_Limit ) {
      let item_Rare = List.find(nftsRare, func(token: Types.Nft) : Bool { token.id == token_id - nfts_Limit });
      switch (item_Rare) {
        case (null) {
          return #Err(#InvalidTokenId);
        };
        case (?token) {
          return #Ok(token.metadata);
        };
      };
    };

    if ( token_id < nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) {
      let item_Epic = List.find(nftsEpic, func(token: Types.Nft) : Bool { token.id == token_id - ( nftsRare_Limit + nfts_Limit ) });
      switch (item_Epic) {
        case (null) {
          return #Err(#InvalidTokenId);
        };
        case (?token) {
          return #Ok(token.metadata);
        };
      };
    };

    if ( token_id < nftsUnique_Limit + nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) {
      let item_Unique = List.find(nftsUnique, func(token: Types.Nft) : Bool { token.id == token_id - ( nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) });
      switch (item_Unique) {
        case (null) {
          return #Err(#InvalidTokenId);
        };
        case (?token) {
          return #Ok(token.metadata);
        };
      };
    };

    if ( token_id < nftsLegendary_Limit + nftsUnique_Limit + nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) {
      let item_Legendary = List.find(nftsLegendary, func(token: Types.Nft) : Bool { token.id == token_id - ( nftsUnique_Limit + nftsEpic_Limit + nftsRare_Limit + nfts_Limit ) });
      switch (item_Legendary) {
        case (null) {
          return #Err(#InvalidTokenId);
        };
        case (?token) {
          return #Ok(token.metadata);
        };
      };
    };

    return #Err(#InvalidTokenId);
  };

  public query func getMaxLimitDip721() : async Nat16 {
    return maxLimit;
  };

  public func setBaseURLCustom(url: Text) : async Text {
    baseURL := url;
    return baseURL;
  };

  // don't need to expansion. this function will check normal NFT's minting or not. 
  public func getMetadataForUserDip721(user: Principal) : async Types.ExtendedMetadataResult {
    let item = List.find(nfts, func(token: Types.Nft) : Bool { token.owner == user });
    switch (item) {
      case null {
        return #Err(#Other);
      };
      case (?token) {
        return #Ok({
          metadata_desc = token.metadata;
          token_id = token.id;
        });
      };
    };

  };

  public query func getTokenIdsForUserDip721(user: Principal) : async [Types.TokenId] {
    
    let items = List.filter(nfts, func(token: Types.Nft) : Bool { token.owner == user });
    let tokenIds = List.map(items, func (item : Types.Nft) : Types.TokenId { item.id });
    let array_tokenIds = List.toArray(tokenIds);

    let itemsRare = List.filter(nftsRare, func(token: Types.Nft) : Bool { token.owner == user });
    let tokenIdsRare = List.map(itemsRare, func (item : Types.Nft) : Types.TokenId { item.id + nfts_Limit });
    let array_tokenIdsRare = List.toArray(tokenIdsRare);
    let array_NormalRare = Array.append(array_tokenIds, array_tokenIdsRare);

    let itemsEpic = List.filter(nftsEpic, func(token: Types.Nft) : Bool { token.owner == user });
    let tokenIdsEpic = List.map(itemsEpic, func (item : Types.Nft) : Types.TokenId { item.id + nfts_Limit + nftsRare_Limit });
    let array_tokenIdsEpic = List.toArray(tokenIdsEpic);
    let array_tillEpic = Array.append(array_NormalRare, array_tokenIdsEpic);

    let itemsUnique = List.filter(nftsUnique, func(token: Types.Nft) : Bool { token.owner == user });
    let tokenIdsUnique = List.map(itemsUnique, func (item : Types.Nft) : Types.TokenId { item.id + nfts_Limit + nftsRare_Limit + nftsEpic_Limit });
    let array_tokenIdsUnique = List.toArray(tokenIdsUnique);
    let array_tillUnique = Array.append(array_tillEpic, array_tokenIdsUnique);

    let itemsLegendary = List.filter(nftsLegendary, func(token: Types.Nft) : Bool { token.owner == user });
    let tokenIdsLegendary = List.map(itemsLegendary, func (item : Types.Nft) : Types.TokenId { item.id + nfts_Limit + nftsRare_Limit + nftsEpic_Limit + nftsUnique_Limit });
    let array_tokenIdsLegendary = List.toArray(tokenIdsLegendary);
    let array_tillLegendary = Array.append(array_tillUnique, array_tokenIdsLegendary);

    return Array.sort(array_tillLegendary, Nat64.compare);
  };

  public shared({ caller }) func mintDip721(to: Principal, metadata: Types.MetadataDesc) : async Types.MintReceipt {

    let newId = Nat64.fromNat(List.size(nfts));

    if ( newId + 1 > nfts_Limit ) {
      return #Err(#ExceedLimit);
    };

    if ( Nat64.fromNat(List.size(List.filter(nfts, func(token: Types.Nft) : Bool { token.owner == to }))) + 1 > 2 ) {
      return #Err(#ExceedLimit);
    };

    let nft : Types.Nft = {
      owner = to;
      id = newId;
      metadata = metadata;
    };

    nfts := List.push(nft, nfts);

    transactionId += 1;

    return #Ok({
      token_id = newId;
      id = transactionId;
    });
  };

  public shared({ caller }) func mintRareCustom(to: Principal, metadata: Types.MetadataDesc) : async Types.MintReceipt {
  
    let newId = Nat64.fromNat(List.size(nftsRare));

    if ( newId + 1 > nftsRare_Limit ) {
      return #Err(#ExceedLimit);
    };

    let item = List.find(nfts, func(token: Types.Nft) : Bool { token.owner == to });
    if ( item == null ) {
        return #Err(#Unauthorized);
    };

    let nft : Types.Nft = {
      owner = to;
      id = newId;
      metadata = metadata;
    };

    nftsRare := List.push(nft, nftsRare);

    transactionId += 1;

    return #Ok({
      token_id = ( newId + nfts_Limit );
      id = transactionId;
    });
  };

  public shared({ caller }) func mintEpicCustom(to: Principal, metadata: Types.MetadataDesc) : async Types.MintReceipt {

    let newId = Nat64.fromNat(List.size(nftsEpic));

    if ( newId + 1 > nftsEpic_Limit ) {
      return #Err(#ExceedLimit);
    };

    let item = List.find(nftsRare, func(token: Types.Nft) : Bool { token.owner == to });
    if ( item == null ) {
        return #Err(#Unauthorized);
    };

    let nft : Types.Nft = {
      owner = to;
      id = newId;
      metadata = metadata;
    };

    nftsEpic := List.push(nft, nftsEpic);

    transactionId += 1;

    return #Ok({
      token_id = newId + nfts_Limit + nftsRare_Limit;
      id = transactionId;
    });
  };

  public shared({ caller }) func mintUniqueCustom(to: Principal, metadata: Types.MetadataDesc) : async Types.MintReceipt {

    let newId = Nat64.fromNat(List.size(nftsUnique));

    if ( newId + 1 > nftsUnique_Limit ) {
      return #Err(#ExceedLimit);
    };

    let item = List.find(nftsEpic, func(token: Types.Nft) : Bool { token.owner == to });
    if ( item == null ) {
        return #Err(#Unauthorized);
    };

    let nft : Types.Nft = {
      owner = to;
      id = newId;
      metadata = metadata;
    };

    nftsUnique := List.push(nft, nftsUnique);

    transactionId += 1;

    return #Ok({
      token_id = newId + nfts_Limit + nftsRare_Limit + nftsEpic_Limit;
      id = transactionId;
    });
  };

  public shared({ caller }) func mintLegendaryCustom(to: Principal, metadata: Types.MetadataDesc) : async Types.MintReceipt {

    let newId = Nat64.fromNat(List.size(nftsLegendary));

    if ( newId + 1 > nftsLegendary_Limit ) {
      return #Err(#ExceedLimit);
    };

    let item = List.find(nftsUnique, func(token: Types.Nft) : Bool { token.owner == to });
    if ( item == null ) {
        return #Err(#Unauthorized);
    };

    let nft : Types.Nft = {
      owner = to;
      id = newId;
      metadata = metadata;
    };

    nftsLegendary := List.push(nft, nftsLegendary);

    transactionId += 1;

    return #Ok({
      token_id = newId + nfts_Limit + nftsRare_Limit + nftsEpic_Limit + nftsUnique_Limit;
      id = transactionId;
    });
  };
}
