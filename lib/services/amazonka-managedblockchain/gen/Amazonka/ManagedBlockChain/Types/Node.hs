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
-- Module      : Amazonka.ManagedBlockChain.Types.Node
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.Node where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.ManagedBlockChain.Types.NodeFrameworkAttributes
import Amazonka.ManagedBlockChain.Types.NodeLogPublishingConfiguration
import Amazonka.ManagedBlockChain.Types.NodeStatus
import Amazonka.ManagedBlockChain.Types.StateDBType
import qualified Amazonka.Prelude as Prelude

-- | Configuration properties of a node.
--
-- /See:/ 'newNode' smart constructor.
data Node = Node'
  { -- | The status of the node.
    --
    -- -   @CREATING@ - The AWS account is in the process of creating a node.
    --
    -- -   @AVAILABLE@ - The node has been created and can participate in the
    --     network.
    --
    -- -   @UNHEALTHY@ - The node is impaired and might not function as
    --     expected. Amazon Managed Blockchain automatically finds nodes in
    --     this state and tries to recover them. If a node is recoverable, it
    --     returns to @AVAILABLE@. Otherwise, it moves to @FAILED@ status.
    --
    -- -   @CREATE_FAILED@ - The AWS account attempted to create a node and
    --     creation failed.
    --
    -- -   @UPDATING@ - The node is in the process of being updated.
    --
    -- -   @DELETING@ - The node is in the process of being deleted.
    --
    -- -   @DELETED@ - The node can no longer participate on the network.
    --
    -- -   @FAILED@ - The node is no longer functional, cannot be recovered,
    --     and must be deleted.
    --
    -- -   @INACCESSIBLE_ENCRYPTION_KEY@ - The node is impaired and might not
    --     function as expected because it cannot access the specified customer
    --     managed key in AWS KMS for encryption at rest. Either the KMS key
    --     was disabled or deleted, or the grants on the key were revoked.
    --
    --     The effect of disabling or deleting a key, or revoking a grant is
    --     not immediate. The node resource might take some time to find that
    --     the key is inaccessible. When a resource is in this state, we
    --     recommend deleting and recreating the resource.
    status :: Prelude.Maybe NodeStatus,
    -- | The Amazon Resource Name (ARN) of the customer managed key in AWS Key
    -- Management Service (AWS KMS) that the node uses for encryption at rest.
    -- If the value of this parameter is @\"AWS Owned KMS Key\"@, the node uses
    -- an AWS owned KMS key for encryption. The node inherits this parameter
    -- from the member that it belongs to.
    --
    -- Applies only to Hyperledger Fabric.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Configuration properties for logging events associated with a peer node
    -- on a Hyperledger Fabric network on Managed Blockchain.
    logPublishingConfiguration :: Prelude.Maybe NodeLogPublishingConfiguration,
    -- | The unique identifier of the member to which the node belongs.
    --
    -- Applies only to Hyperledger Fabric.
    memberId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the node. For more information about
    -- ARNs and their format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the network that the node is on.
    networkId :: Prelude.Maybe Prelude.Text,
    -- | The instance type of the node.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The state database that the node uses. Values are @LevelDB@ or
    -- @CouchDB@.
    --
    -- Applies only to Hyperledger Fabric.
    stateDB :: Prelude.Maybe StateDBType,
    -- | The Availability Zone in which the node exists. Required for Ethereum
    -- nodes.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the node.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the node was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | Attributes of the blockchain framework being used.
    frameworkAttributes :: Prelude.Maybe NodeFrameworkAttributes,
    -- | Tags assigned to the node. Each tag consists of a key and optional
    -- value.
    --
    -- For more information about tags, see
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
    -- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
    -- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Node' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'node_status' - The status of the node.
--
-- -   @CREATING@ - The AWS account is in the process of creating a node.
--
-- -   @AVAILABLE@ - The node has been created and can participate in the
--     network.
--
-- -   @UNHEALTHY@ - The node is impaired and might not function as
--     expected. Amazon Managed Blockchain automatically finds nodes in
--     this state and tries to recover them. If a node is recoverable, it
--     returns to @AVAILABLE@. Otherwise, it moves to @FAILED@ status.
--
-- -   @CREATE_FAILED@ - The AWS account attempted to create a node and
--     creation failed.
--
-- -   @UPDATING@ - The node is in the process of being updated.
--
-- -   @DELETING@ - The node is in the process of being deleted.
--
-- -   @DELETED@ - The node can no longer participate on the network.
--
-- -   @FAILED@ - The node is no longer functional, cannot be recovered,
--     and must be deleted.
--
-- -   @INACCESSIBLE_ENCRYPTION_KEY@ - The node is impaired and might not
--     function as expected because it cannot access the specified customer
--     managed key in AWS KMS for encryption at rest. Either the KMS key
--     was disabled or deleted, or the grants on the key were revoked.
--
--     The effect of disabling or deleting a key, or revoking a grant is
--     not immediate. The node resource might take some time to find that
--     the key is inaccessible. When a resource is in this state, we
--     recommend deleting and recreating the resource.
--
-- 'kmsKeyArn', 'node_kmsKeyArn' - The Amazon Resource Name (ARN) of the customer managed key in AWS Key
-- Management Service (AWS KMS) that the node uses for encryption at rest.
-- If the value of this parameter is @\"AWS Owned KMS Key\"@, the node uses
-- an AWS owned KMS key for encryption. The node inherits this parameter
-- from the member that it belongs to.
--
-- Applies only to Hyperledger Fabric.
--
-- 'logPublishingConfiguration', 'node_logPublishingConfiguration' - Configuration properties for logging events associated with a peer node
-- on a Hyperledger Fabric network on Managed Blockchain.
--
-- 'memberId', 'node_memberId' - The unique identifier of the member to which the node belongs.
--
-- Applies only to Hyperledger Fabric.
--
-- 'arn', 'node_arn' - The Amazon Resource Name (ARN) of the node. For more information about
-- ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'networkId', 'node_networkId' - The unique identifier of the network that the node is on.
--
-- 'instanceType', 'node_instanceType' - The instance type of the node.
--
-- 'stateDB', 'node_stateDB' - The state database that the node uses. Values are @LevelDB@ or
-- @CouchDB@.
--
-- Applies only to Hyperledger Fabric.
--
-- 'availabilityZone', 'node_availabilityZone' - The Availability Zone in which the node exists. Required for Ethereum
-- nodes.
--
-- 'id', 'node_id' - The unique identifier of the node.
--
-- 'creationDate', 'node_creationDate' - The date and time that the node was created.
--
-- 'frameworkAttributes', 'node_frameworkAttributes' - Attributes of the blockchain framework being used.
--
-- 'tags', 'node_tags' - Tags assigned to the node. Each tag consists of a key and optional
-- value.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
newNode ::
  Node
newNode =
  Node'
    { status = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      logPublishingConfiguration = Prelude.Nothing,
      memberId = Prelude.Nothing,
      arn = Prelude.Nothing,
      networkId = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      stateDB = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      id = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      frameworkAttributes = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The status of the node.
--
-- -   @CREATING@ - The AWS account is in the process of creating a node.
--
-- -   @AVAILABLE@ - The node has been created and can participate in the
--     network.
--
-- -   @UNHEALTHY@ - The node is impaired and might not function as
--     expected. Amazon Managed Blockchain automatically finds nodes in
--     this state and tries to recover them. If a node is recoverable, it
--     returns to @AVAILABLE@. Otherwise, it moves to @FAILED@ status.
--
-- -   @CREATE_FAILED@ - The AWS account attempted to create a node and
--     creation failed.
--
-- -   @UPDATING@ - The node is in the process of being updated.
--
-- -   @DELETING@ - The node is in the process of being deleted.
--
-- -   @DELETED@ - The node can no longer participate on the network.
--
-- -   @FAILED@ - The node is no longer functional, cannot be recovered,
--     and must be deleted.
--
-- -   @INACCESSIBLE_ENCRYPTION_KEY@ - The node is impaired and might not
--     function as expected because it cannot access the specified customer
--     managed key in AWS KMS for encryption at rest. Either the KMS key
--     was disabled or deleted, or the grants on the key were revoked.
--
--     The effect of disabling or deleting a key, or revoking a grant is
--     not immediate. The node resource might take some time to find that
--     the key is inaccessible. When a resource is in this state, we
--     recommend deleting and recreating the resource.
node_status :: Lens.Lens' Node (Prelude.Maybe NodeStatus)
node_status = Lens.lens (\Node' {status} -> status) (\s@Node' {} a -> s {status = a} :: Node)

-- | The Amazon Resource Name (ARN) of the customer managed key in AWS Key
-- Management Service (AWS KMS) that the node uses for encryption at rest.
-- If the value of this parameter is @\"AWS Owned KMS Key\"@, the node uses
-- an AWS owned KMS key for encryption. The node inherits this parameter
-- from the member that it belongs to.
--
-- Applies only to Hyperledger Fabric.
node_kmsKeyArn :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_kmsKeyArn = Lens.lens (\Node' {kmsKeyArn} -> kmsKeyArn) (\s@Node' {} a -> s {kmsKeyArn = a} :: Node)

-- | Configuration properties for logging events associated with a peer node
-- on a Hyperledger Fabric network on Managed Blockchain.
node_logPublishingConfiguration :: Lens.Lens' Node (Prelude.Maybe NodeLogPublishingConfiguration)
node_logPublishingConfiguration = Lens.lens (\Node' {logPublishingConfiguration} -> logPublishingConfiguration) (\s@Node' {} a -> s {logPublishingConfiguration = a} :: Node)

-- | The unique identifier of the member to which the node belongs.
--
-- Applies only to Hyperledger Fabric.
node_memberId :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_memberId = Lens.lens (\Node' {memberId} -> memberId) (\s@Node' {} a -> s {memberId = a} :: Node)

-- | The Amazon Resource Name (ARN) of the node. For more information about
-- ARNs and their format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
node_arn :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_arn = Lens.lens (\Node' {arn} -> arn) (\s@Node' {} a -> s {arn = a} :: Node)

-- | The unique identifier of the network that the node is on.
node_networkId :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_networkId = Lens.lens (\Node' {networkId} -> networkId) (\s@Node' {} a -> s {networkId = a} :: Node)

-- | The instance type of the node.
node_instanceType :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_instanceType = Lens.lens (\Node' {instanceType} -> instanceType) (\s@Node' {} a -> s {instanceType = a} :: Node)

-- | The state database that the node uses. Values are @LevelDB@ or
-- @CouchDB@.
--
-- Applies only to Hyperledger Fabric.
node_stateDB :: Lens.Lens' Node (Prelude.Maybe StateDBType)
node_stateDB = Lens.lens (\Node' {stateDB} -> stateDB) (\s@Node' {} a -> s {stateDB = a} :: Node)

-- | The Availability Zone in which the node exists. Required for Ethereum
-- nodes.
node_availabilityZone :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_availabilityZone = Lens.lens (\Node' {availabilityZone} -> availabilityZone) (\s@Node' {} a -> s {availabilityZone = a} :: Node)

-- | The unique identifier of the node.
node_id :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_id = Lens.lens (\Node' {id} -> id) (\s@Node' {} a -> s {id = a} :: Node)

-- | The date and time that the node was created.
node_creationDate :: Lens.Lens' Node (Prelude.Maybe Prelude.UTCTime)
node_creationDate = Lens.lens (\Node' {creationDate} -> creationDate) (\s@Node' {} a -> s {creationDate = a} :: Node) Prelude.. Lens.mapping Core._Time

-- | Attributes of the blockchain framework being used.
node_frameworkAttributes :: Lens.Lens' Node (Prelude.Maybe NodeFrameworkAttributes)
node_frameworkAttributes = Lens.lens (\Node' {frameworkAttributes} -> frameworkAttributes) (\s@Node' {} a -> s {frameworkAttributes = a} :: Node)

-- | Tags assigned to the node. Each tag consists of a key and optional
-- value.
--
-- For more information about tags, see
-- <https://docs.aws.amazon.com/managed-blockchain/latest/ethereum-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Ethereum Developer Guide/, or
-- <https://docs.aws.amazon.com/managed-blockchain/latest/hyperledger-fabric-dev/tagging-resources.html Tagging Resources>
-- in the /Amazon Managed Blockchain Hyperledger Fabric Developer Guide/.
node_tags :: Lens.Lens' Node (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
node_tags = Lens.lens (\Node' {tags} -> tags) (\s@Node' {} a -> s {tags = a} :: Node) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Node where
  parseJSON =
    Core.withObject
      "Node"
      ( \x ->
          Node'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "KmsKeyArn")
            Prelude.<*> (x Core..:? "LogPublishingConfiguration")
            Prelude.<*> (x Core..:? "MemberId")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "NetworkId")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "StateDB")
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "FrameworkAttributes")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Node where
  hashWithSalt salt' Node' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` frameworkAttributes
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` stateDB
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` logPublishingConfiguration
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData Node where
  rnf Node' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf frameworkAttributes
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf stateDB
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf logPublishingConfiguration
      `Prelude.seq` Prelude.rnf kmsKeyArn
