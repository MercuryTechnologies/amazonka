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
-- Module      : Amazonka.CloudHSMV2.Types.Cluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudHSMV2.Types.Cluster where

import Amazonka.CloudHSMV2.Types.BackupPolicy
import Amazonka.CloudHSMV2.Types.BackupRetentionPolicy
import Amazonka.CloudHSMV2.Types.Certificates
import Amazonka.CloudHSMV2.Types.ClusterState
import Amazonka.CloudHSMV2.Types.Hsm
import Amazonka.CloudHSMV2.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an AWS CloudHSM cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The default password for the cluster\'s Pre-Crypto Officer (PRECO) user.
    preCoPassword :: Prelude.Maybe Prelude.Text,
    -- | A description of the cluster\'s state.
    stateMessage :: Prelude.Maybe Prelude.Text,
    -- | The cluster\'s state.
    state :: Prelude.Maybe ClusterState,
    -- | A map from availability zone to the cluster’s subnet in that
    -- availability zone.
    subnetMapping :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A policy that defines how the service retains backups.
    backupRetentionPolicy :: Prelude.Maybe BackupRetentionPolicy,
    -- | Contains information about the HSMs in the cluster.
    hsms :: Prelude.Maybe [Hsm],
    -- | The identifier (ID) of the virtual private cloud (VPC) that contains the
    -- cluster.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The list of tags for the cluster.
    tagList :: Prelude.Maybe [Tag],
    -- | The identifier (ID) of the backup used to create the cluster. This value
    -- exists only when the cluster was created from a backup.
    sourceBackupId :: Prelude.Maybe Prelude.Text,
    -- | Contains one or more certificates or a certificate signing request
    -- (CSR).
    certificates :: Prelude.Maybe Certificates,
    -- | The identifier (ID) of the cluster\'s security group.
    securityGroup :: Prelude.Maybe Prelude.Text,
    -- | The cluster\'s identifier (ID).
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the cluster was created.
    createTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The cluster\'s backup policy.
    backupPolicy :: Prelude.Maybe BackupPolicy,
    -- | The type of HSM that the cluster contains.
    hsmType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preCoPassword', 'cluster_preCoPassword' - The default password for the cluster\'s Pre-Crypto Officer (PRECO) user.
--
-- 'stateMessage', 'cluster_stateMessage' - A description of the cluster\'s state.
--
-- 'state', 'cluster_state' - The cluster\'s state.
--
-- 'subnetMapping', 'cluster_subnetMapping' - A map from availability zone to the cluster’s subnet in that
-- availability zone.
--
-- 'backupRetentionPolicy', 'cluster_backupRetentionPolicy' - A policy that defines how the service retains backups.
--
-- 'hsms', 'cluster_hsms' - Contains information about the HSMs in the cluster.
--
-- 'vpcId', 'cluster_vpcId' - The identifier (ID) of the virtual private cloud (VPC) that contains the
-- cluster.
--
-- 'tagList', 'cluster_tagList' - The list of tags for the cluster.
--
-- 'sourceBackupId', 'cluster_sourceBackupId' - The identifier (ID) of the backup used to create the cluster. This value
-- exists only when the cluster was created from a backup.
--
-- 'certificates', 'cluster_certificates' - Contains one or more certificates or a certificate signing request
-- (CSR).
--
-- 'securityGroup', 'cluster_securityGroup' - The identifier (ID) of the cluster\'s security group.
--
-- 'clusterId', 'cluster_clusterId' - The cluster\'s identifier (ID).
--
-- 'createTimestamp', 'cluster_createTimestamp' - The date and time when the cluster was created.
--
-- 'backupPolicy', 'cluster_backupPolicy' - The cluster\'s backup policy.
--
-- 'hsmType', 'cluster_hsmType' - The type of HSM that the cluster contains.
newCluster ::
  Cluster
newCluster =
  Cluster'
    { preCoPassword = Prelude.Nothing,
      stateMessage = Prelude.Nothing,
      state = Prelude.Nothing,
      subnetMapping = Prelude.Nothing,
      backupRetentionPolicy = Prelude.Nothing,
      hsms = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      tagList = Prelude.Nothing,
      sourceBackupId = Prelude.Nothing,
      certificates = Prelude.Nothing,
      securityGroup = Prelude.Nothing,
      clusterId = Prelude.Nothing,
      createTimestamp = Prelude.Nothing,
      backupPolicy = Prelude.Nothing,
      hsmType = Prelude.Nothing
    }

-- | The default password for the cluster\'s Pre-Crypto Officer (PRECO) user.
cluster_preCoPassword :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_preCoPassword = Lens.lens (\Cluster' {preCoPassword} -> preCoPassword) (\s@Cluster' {} a -> s {preCoPassword = a} :: Cluster)

-- | A description of the cluster\'s state.
cluster_stateMessage :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_stateMessage = Lens.lens (\Cluster' {stateMessage} -> stateMessage) (\s@Cluster' {} a -> s {stateMessage = a} :: Cluster)

-- | The cluster\'s state.
cluster_state :: Lens.Lens' Cluster (Prelude.Maybe ClusterState)
cluster_state = Lens.lens (\Cluster' {state} -> state) (\s@Cluster' {} a -> s {state = a} :: Cluster)

-- | A map from availability zone to the cluster’s subnet in that
-- availability zone.
cluster_subnetMapping :: Lens.Lens' Cluster (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
cluster_subnetMapping = Lens.lens (\Cluster' {subnetMapping} -> subnetMapping) (\s@Cluster' {} a -> s {subnetMapping = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | A policy that defines how the service retains backups.
cluster_backupRetentionPolicy :: Lens.Lens' Cluster (Prelude.Maybe BackupRetentionPolicy)
cluster_backupRetentionPolicy = Lens.lens (\Cluster' {backupRetentionPolicy} -> backupRetentionPolicy) (\s@Cluster' {} a -> s {backupRetentionPolicy = a} :: Cluster)

-- | Contains information about the HSMs in the cluster.
cluster_hsms :: Lens.Lens' Cluster (Prelude.Maybe [Hsm])
cluster_hsms = Lens.lens (\Cluster' {hsms} -> hsms) (\s@Cluster' {} a -> s {hsms = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The identifier (ID) of the virtual private cloud (VPC) that contains the
-- cluster.
cluster_vpcId :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_vpcId = Lens.lens (\Cluster' {vpcId} -> vpcId) (\s@Cluster' {} a -> s {vpcId = a} :: Cluster)

-- | The list of tags for the cluster.
cluster_tagList :: Lens.Lens' Cluster (Prelude.Maybe [Tag])
cluster_tagList = Lens.lens (\Cluster' {tagList} -> tagList) (\s@Cluster' {} a -> s {tagList = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The identifier (ID) of the backup used to create the cluster. This value
-- exists only when the cluster was created from a backup.
cluster_sourceBackupId :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_sourceBackupId = Lens.lens (\Cluster' {sourceBackupId} -> sourceBackupId) (\s@Cluster' {} a -> s {sourceBackupId = a} :: Cluster)

-- | Contains one or more certificates or a certificate signing request
-- (CSR).
cluster_certificates :: Lens.Lens' Cluster (Prelude.Maybe Certificates)
cluster_certificates = Lens.lens (\Cluster' {certificates} -> certificates) (\s@Cluster' {} a -> s {certificates = a} :: Cluster)

-- | The identifier (ID) of the cluster\'s security group.
cluster_securityGroup :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_securityGroup = Lens.lens (\Cluster' {securityGroup} -> securityGroup) (\s@Cluster' {} a -> s {securityGroup = a} :: Cluster)

-- | The cluster\'s identifier (ID).
cluster_clusterId :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_clusterId = Lens.lens (\Cluster' {clusterId} -> clusterId) (\s@Cluster' {} a -> s {clusterId = a} :: Cluster)

-- | The date and time when the cluster was created.
cluster_createTimestamp :: Lens.Lens' Cluster (Prelude.Maybe Prelude.UTCTime)
cluster_createTimestamp = Lens.lens (\Cluster' {createTimestamp} -> createTimestamp) (\s@Cluster' {} a -> s {createTimestamp = a} :: Cluster) Prelude.. Lens.mapping Core._Time

-- | The cluster\'s backup policy.
cluster_backupPolicy :: Lens.Lens' Cluster (Prelude.Maybe BackupPolicy)
cluster_backupPolicy = Lens.lens (\Cluster' {backupPolicy} -> backupPolicy) (\s@Cluster' {} a -> s {backupPolicy = a} :: Cluster)

-- | The type of HSM that the cluster contains.
cluster_hsmType :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_hsmType = Lens.lens (\Cluster' {hsmType} -> hsmType) (\s@Cluster' {} a -> s {hsmType = a} :: Cluster)

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Prelude.<$> (x Core..:? "PreCoPassword")
            Prelude.<*> (x Core..:? "StateMessage")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "SubnetMapping" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "BackupRetentionPolicy")
            Prelude.<*> (x Core..:? "Hsms" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> (x Core..:? "TagList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "SourceBackupId")
            Prelude.<*> (x Core..:? "Certificates")
            Prelude.<*> (x Core..:? "SecurityGroup")
            Prelude.<*> (x Core..:? "ClusterId")
            Prelude.<*> (x Core..:? "CreateTimestamp")
            Prelude.<*> (x Core..:? "BackupPolicy")
            Prelude.<*> (x Core..:? "HsmType")
      )

instance Prelude.Hashable Cluster where
  hashWithSalt salt' Cluster' {..} =
    salt' `Prelude.hashWithSalt` hsmType
      `Prelude.hashWithSalt` backupPolicy
      `Prelude.hashWithSalt` createTimestamp
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` securityGroup
      `Prelude.hashWithSalt` certificates
      `Prelude.hashWithSalt` sourceBackupId
      `Prelude.hashWithSalt` tagList
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` hsms
      `Prelude.hashWithSalt` backupRetentionPolicy
      `Prelude.hashWithSalt` subnetMapping
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateMessage
      `Prelude.hashWithSalt` preCoPassword

instance Prelude.NFData Cluster where
  rnf Cluster' {..} =
    Prelude.rnf preCoPassword
      `Prelude.seq` Prelude.rnf hsmType
      `Prelude.seq` Prelude.rnf backupPolicy
      `Prelude.seq` Prelude.rnf createTimestamp
      `Prelude.seq` Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf securityGroup
      `Prelude.seq` Prelude.rnf certificates
      `Prelude.seq` Prelude.rnf sourceBackupId
      `Prelude.seq` Prelude.rnf tagList
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf hsms
      `Prelude.seq` Prelude.rnf backupRetentionPolicy
      `Prelude.seq` Prelude.rnf subnetMapping
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateMessage
