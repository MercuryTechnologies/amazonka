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
-- Module      : Amazonka.ManagedBlockChain.Types.NetworkFrameworkAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NetworkFrameworkAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.ManagedBlockChain.Types.NetworkEthereumAttributes
import Amazonka.ManagedBlockChain.Types.NetworkFabricAttributes
import qualified Amazonka.Prelude as Prelude

-- | Attributes relevant to the network for the blockchain framework that the
-- network uses.
--
-- /See:/ 'newNetworkFrameworkAttributes' smart constructor.
data NetworkFrameworkAttributes = NetworkFrameworkAttributes'
  { -- | Attributes of Hyperledger Fabric for a Managed Blockchain network that
    -- uses Hyperledger Fabric.
    fabric :: Prelude.Maybe NetworkFabricAttributes,
    -- | Attributes of an Ethereum network for Managed Blockchain resources
    -- participating in an Ethereum network.
    ethereum :: Prelude.Maybe NetworkEthereumAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkFrameworkAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fabric', 'networkFrameworkAttributes_fabric' - Attributes of Hyperledger Fabric for a Managed Blockchain network that
-- uses Hyperledger Fabric.
--
-- 'ethereum', 'networkFrameworkAttributes_ethereum' - Attributes of an Ethereum network for Managed Blockchain resources
-- participating in an Ethereum network.
newNetworkFrameworkAttributes ::
  NetworkFrameworkAttributes
newNetworkFrameworkAttributes =
  NetworkFrameworkAttributes'
    { fabric =
        Prelude.Nothing,
      ethereum = Prelude.Nothing
    }

-- | Attributes of Hyperledger Fabric for a Managed Blockchain network that
-- uses Hyperledger Fabric.
networkFrameworkAttributes_fabric :: Lens.Lens' NetworkFrameworkAttributes (Prelude.Maybe NetworkFabricAttributes)
networkFrameworkAttributes_fabric = Lens.lens (\NetworkFrameworkAttributes' {fabric} -> fabric) (\s@NetworkFrameworkAttributes' {} a -> s {fabric = a} :: NetworkFrameworkAttributes)

-- | Attributes of an Ethereum network for Managed Blockchain resources
-- participating in an Ethereum network.
networkFrameworkAttributes_ethereum :: Lens.Lens' NetworkFrameworkAttributes (Prelude.Maybe NetworkEthereumAttributes)
networkFrameworkAttributes_ethereum = Lens.lens (\NetworkFrameworkAttributes' {ethereum} -> ethereum) (\s@NetworkFrameworkAttributes' {} a -> s {ethereum = a} :: NetworkFrameworkAttributes)

instance Core.FromJSON NetworkFrameworkAttributes where
  parseJSON =
    Core.withObject
      "NetworkFrameworkAttributes"
      ( \x ->
          NetworkFrameworkAttributes'
            Prelude.<$> (x Core..:? "Fabric")
            Prelude.<*> (x Core..:? "Ethereum")
      )

instance Prelude.Hashable NetworkFrameworkAttributes where
  hashWithSalt salt' NetworkFrameworkAttributes' {..} =
    salt' `Prelude.hashWithSalt` ethereum
      `Prelude.hashWithSalt` fabric

instance Prelude.NFData NetworkFrameworkAttributes where
  rnf NetworkFrameworkAttributes' {..} =
    Prelude.rnf fabric
      `Prelude.seq` Prelude.rnf ethereum
