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
-- Module      : Amazonka.SecretsManager.Types.ReplicationStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Types.ReplicationStatusType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecretsManager.Types.StatusType

-- | A replication object consisting of a @RegionReplicationStatus@ object
-- and includes a Region, KMSKeyId, status, and status message.
--
-- /See:/ 'newReplicationStatusType' smart constructor.
data ReplicationStatusType = ReplicationStatusType'
  { -- | The status can be @InProgress@, @Failed@, or @InSync@.
    status :: Prelude.Maybe StatusType,
    -- | Can be an @ARN@, @Key ID@, or @Alias@.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Status message such as \"/Secret with this name already exists in this
    -- region/\".
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The Region where replication occurs.
    region :: Prelude.Maybe Prelude.Text,
    -- | The date that you last accessed the secret in the Region.
    lastAccessedDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationStatusType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'replicationStatusType_status' - The status can be @InProgress@, @Failed@, or @InSync@.
--
-- 'kmsKeyId', 'replicationStatusType_kmsKeyId' - Can be an @ARN@, @Key ID@, or @Alias@.
--
-- 'statusMessage', 'replicationStatusType_statusMessage' - Status message such as \"/Secret with this name already exists in this
-- region/\".
--
-- 'region', 'replicationStatusType_region' - The Region where replication occurs.
--
-- 'lastAccessedDate', 'replicationStatusType_lastAccessedDate' - The date that you last accessed the secret in the Region.
newReplicationStatusType ::
  ReplicationStatusType
newReplicationStatusType =
  ReplicationStatusType'
    { status = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      region = Prelude.Nothing,
      lastAccessedDate = Prelude.Nothing
    }

-- | The status can be @InProgress@, @Failed@, or @InSync@.
replicationStatusType_status :: Lens.Lens' ReplicationStatusType (Prelude.Maybe StatusType)
replicationStatusType_status = Lens.lens (\ReplicationStatusType' {status} -> status) (\s@ReplicationStatusType' {} a -> s {status = a} :: ReplicationStatusType)

-- | Can be an @ARN@, @Key ID@, or @Alias@.
replicationStatusType_kmsKeyId :: Lens.Lens' ReplicationStatusType (Prelude.Maybe Prelude.Text)
replicationStatusType_kmsKeyId = Lens.lens (\ReplicationStatusType' {kmsKeyId} -> kmsKeyId) (\s@ReplicationStatusType' {} a -> s {kmsKeyId = a} :: ReplicationStatusType)

-- | Status message such as \"/Secret with this name already exists in this
-- region/\".
replicationStatusType_statusMessage :: Lens.Lens' ReplicationStatusType (Prelude.Maybe Prelude.Text)
replicationStatusType_statusMessage = Lens.lens (\ReplicationStatusType' {statusMessage} -> statusMessage) (\s@ReplicationStatusType' {} a -> s {statusMessage = a} :: ReplicationStatusType)

-- | The Region where replication occurs.
replicationStatusType_region :: Lens.Lens' ReplicationStatusType (Prelude.Maybe Prelude.Text)
replicationStatusType_region = Lens.lens (\ReplicationStatusType' {region} -> region) (\s@ReplicationStatusType' {} a -> s {region = a} :: ReplicationStatusType)

-- | The date that you last accessed the secret in the Region.
replicationStatusType_lastAccessedDate :: Lens.Lens' ReplicationStatusType (Prelude.Maybe Prelude.UTCTime)
replicationStatusType_lastAccessedDate = Lens.lens (\ReplicationStatusType' {lastAccessedDate} -> lastAccessedDate) (\s@ReplicationStatusType' {} a -> s {lastAccessedDate = a} :: ReplicationStatusType) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ReplicationStatusType where
  parseJSON =
    Core.withObject
      "ReplicationStatusType"
      ( \x ->
          ReplicationStatusType'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "Region")
            Prelude.<*> (x Core..:? "LastAccessedDate")
      )

instance Prelude.Hashable ReplicationStatusType where
  hashWithSalt salt' ReplicationStatusType' {..} =
    salt' `Prelude.hashWithSalt` lastAccessedDate
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` status

instance Prelude.NFData ReplicationStatusType where
  rnf ReplicationStatusType' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastAccessedDate
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf kmsKeyId
