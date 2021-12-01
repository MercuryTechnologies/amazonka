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
-- Module      : Amazonka.ManagedBlockChain.Types.NodeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.NodeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.ManagedBlockChain.Types.NodeLogPublishingConfiguration
import Amazonka.ManagedBlockChain.Types.StateDBType
import qualified Amazonka.Prelude as Prelude

-- | Configuration properties of a node.
--
-- /See:/ 'newNodeConfiguration' smart constructor.
data NodeConfiguration = NodeConfiguration'
  { -- | Configuration properties for logging events associated with a peer node
    -- on a Hyperledger Fabric network on Managed Blockchain.
    logPublishingConfiguration :: Prelude.Maybe NodeLogPublishingConfiguration,
    -- | The state database that the node uses. Values are @LevelDB@ or
    -- @CouchDB@. When using an Amazon Managed Blockchain network with
    -- Hyperledger Fabric version 1.4 or later, the default is @CouchDB@.
    --
    -- Applies only to Hyperledger Fabric.
    stateDB :: Prelude.Maybe StateDBType,
    -- | The Availability Zone in which the node exists. Required for Ethereum
    -- nodes.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Managed Blockchain instance type for the node.
    instanceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logPublishingConfiguration', 'nodeConfiguration_logPublishingConfiguration' - Configuration properties for logging events associated with a peer node
-- on a Hyperledger Fabric network on Managed Blockchain.
--
-- 'stateDB', 'nodeConfiguration_stateDB' - The state database that the node uses. Values are @LevelDB@ or
-- @CouchDB@. When using an Amazon Managed Blockchain network with
-- Hyperledger Fabric version 1.4 or later, the default is @CouchDB@.
--
-- Applies only to Hyperledger Fabric.
--
-- 'availabilityZone', 'nodeConfiguration_availabilityZone' - The Availability Zone in which the node exists. Required for Ethereum
-- nodes.
--
-- 'instanceType', 'nodeConfiguration_instanceType' - The Amazon Managed Blockchain instance type for the node.
newNodeConfiguration ::
  -- | 'instanceType'
  Prelude.Text ->
  NodeConfiguration
newNodeConfiguration pInstanceType_ =
  NodeConfiguration'
    { logPublishingConfiguration =
        Prelude.Nothing,
      stateDB = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      instanceType = pInstanceType_
    }

-- | Configuration properties for logging events associated with a peer node
-- on a Hyperledger Fabric network on Managed Blockchain.
nodeConfiguration_logPublishingConfiguration :: Lens.Lens' NodeConfiguration (Prelude.Maybe NodeLogPublishingConfiguration)
nodeConfiguration_logPublishingConfiguration = Lens.lens (\NodeConfiguration' {logPublishingConfiguration} -> logPublishingConfiguration) (\s@NodeConfiguration' {} a -> s {logPublishingConfiguration = a} :: NodeConfiguration)

-- | The state database that the node uses. Values are @LevelDB@ or
-- @CouchDB@. When using an Amazon Managed Blockchain network with
-- Hyperledger Fabric version 1.4 or later, the default is @CouchDB@.
--
-- Applies only to Hyperledger Fabric.
nodeConfiguration_stateDB :: Lens.Lens' NodeConfiguration (Prelude.Maybe StateDBType)
nodeConfiguration_stateDB = Lens.lens (\NodeConfiguration' {stateDB} -> stateDB) (\s@NodeConfiguration' {} a -> s {stateDB = a} :: NodeConfiguration)

-- | The Availability Zone in which the node exists. Required for Ethereum
-- nodes.
nodeConfiguration_availabilityZone :: Lens.Lens' NodeConfiguration (Prelude.Maybe Prelude.Text)
nodeConfiguration_availabilityZone = Lens.lens (\NodeConfiguration' {availabilityZone} -> availabilityZone) (\s@NodeConfiguration' {} a -> s {availabilityZone = a} :: NodeConfiguration)

-- | The Amazon Managed Blockchain instance type for the node.
nodeConfiguration_instanceType :: Lens.Lens' NodeConfiguration Prelude.Text
nodeConfiguration_instanceType = Lens.lens (\NodeConfiguration' {instanceType} -> instanceType) (\s@NodeConfiguration' {} a -> s {instanceType = a} :: NodeConfiguration)

instance Prelude.Hashable NodeConfiguration where
  hashWithSalt salt' NodeConfiguration' {..} =
    salt' `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` stateDB
      `Prelude.hashWithSalt` logPublishingConfiguration

instance Prelude.NFData NodeConfiguration where
  rnf NodeConfiguration' {..} =
    Prelude.rnf logPublishingConfiguration
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf stateDB

instance Core.ToJSON NodeConfiguration where
  toJSON NodeConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LogPublishingConfiguration" Core..=)
              Prelude.<$> logPublishingConfiguration,
            ("StateDB" Core..=) Prelude.<$> stateDB,
            ("AvailabilityZone" Core..=)
              Prelude.<$> availabilityZone,
            Prelude.Just ("InstanceType" Core..= instanceType)
          ]
      )
