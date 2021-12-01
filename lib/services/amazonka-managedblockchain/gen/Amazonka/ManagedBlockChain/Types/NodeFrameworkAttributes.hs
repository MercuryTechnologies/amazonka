{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ManagedBlockChain.Types.NodeFrameworkAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NodeFrameworkAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.ManagedBlockChain.Types.NodeEthereumAttributes
import Amazonka.ManagedBlockChain.Types.NodeFabricAttributes
import qualified Amazonka.Prelude as Prelude

-- | Attributes relevant to a node on a Managed Blockchain network for the
-- blockchain framework that the network uses.
--
-- /See:/ 'newNodeFrameworkAttributes' smart constructor.
data NodeFrameworkAttributes = NodeFrameworkAttributes'
  { -- | Attributes of Hyperledger Fabric for a peer node on a Managed Blockchain
    -- network that uses Hyperledger Fabric.
    fabric :: Prelude.Maybe NodeFabricAttributes,
    -- | Attributes of Ethereum for a node on a Managed Blockchain network that
    -- uses Ethereum.
    ethereum :: Prelude.Maybe NodeEthereumAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeFrameworkAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fabric', 'nodeFrameworkAttributes_fabric' - Attributes of Hyperledger Fabric for a peer node on a Managed Blockchain
-- network that uses Hyperledger Fabric.
--
-- 'ethereum', 'nodeFrameworkAttributes_ethereum' - Attributes of Ethereum for a node on a Managed Blockchain network that
-- uses Ethereum.
newNodeFrameworkAttributes ::
  NodeFrameworkAttributes
newNodeFrameworkAttributes =
  NodeFrameworkAttributes'
    { fabric = Prelude.Nothing,
      ethereum = Prelude.Nothing
    }

-- | Attributes of Hyperledger Fabric for a peer node on a Managed Blockchain
-- network that uses Hyperledger Fabric.
nodeFrameworkAttributes_fabric :: Lens.Lens' NodeFrameworkAttributes (Prelude.Maybe NodeFabricAttributes)
nodeFrameworkAttributes_fabric = Lens.lens (\NodeFrameworkAttributes' {fabric} -> fabric) (\s@NodeFrameworkAttributes' {} a -> s {fabric = a} :: NodeFrameworkAttributes)

-- | Attributes of Ethereum for a node on a Managed Blockchain network that
-- uses Ethereum.
nodeFrameworkAttributes_ethereum :: Lens.Lens' NodeFrameworkAttributes (Prelude.Maybe NodeEthereumAttributes)
nodeFrameworkAttributes_ethereum = Lens.lens (\NodeFrameworkAttributes' {ethereum} -> ethereum) (\s@NodeFrameworkAttributes' {} a -> s {ethereum = a} :: NodeFrameworkAttributes)

instance Core.FromJSON NodeFrameworkAttributes where
  parseJSON =
    Core.withObject
      "NodeFrameworkAttributes"
      ( \x ->
          NodeFrameworkAttributes'
            Prelude.<$> (x Core..:? "Fabric")
            Prelude.<*> (x Core..:? "Ethereum")
      )

instance Prelude.Hashable NodeFrameworkAttributes where
  hashWithSalt salt' NodeFrameworkAttributes' {..} =
    salt' `Prelude.hashWithSalt` ethereum
      `Prelude.hashWithSalt` fabric

instance Prelude.NFData NodeFrameworkAttributes where
  rnf NodeFrameworkAttributes' {..} =
    Prelude.rnf fabric
      `Prelude.seq` Prelude.rnf ethereum
