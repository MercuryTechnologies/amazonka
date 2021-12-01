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
-- Module      : Amazonka.DMS.Types.RefreshSchemasStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.RefreshSchemasStatus where

import qualified Amazonka.Core as Core
import Amazonka.DMS.Types.RefreshSchemasStatusTypeValue
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information that describes status of a schema at an endpoint
-- specified by the @DescribeRefreshSchemaStatus@ operation.
--
-- /See:/ 'newRefreshSchemasStatus' smart constructor.
data RefreshSchemasStatus = RefreshSchemasStatus'
  { -- | The status of the schema.
    status :: Prelude.Maybe RefreshSchemasStatusTypeValue,
    -- | The date the schema was last refreshed.
    lastRefreshDate :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Prelude.Maybe Prelude.Text,
    -- | The last failure message for the schema.
    lastFailureMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RefreshSchemasStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'refreshSchemasStatus_status' - The status of the schema.
--
-- 'lastRefreshDate', 'refreshSchemasStatus_lastRefreshDate' - The date the schema was last refreshed.
--
-- 'replicationInstanceArn', 'refreshSchemasStatus_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
--
-- 'endpointArn', 'refreshSchemasStatus_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
--
-- 'lastFailureMessage', 'refreshSchemasStatus_lastFailureMessage' - The last failure message for the schema.
newRefreshSchemasStatus ::
  RefreshSchemasStatus
newRefreshSchemasStatus =
  RefreshSchemasStatus'
    { status = Prelude.Nothing,
      lastRefreshDate = Prelude.Nothing,
      replicationInstanceArn = Prelude.Nothing,
      endpointArn = Prelude.Nothing,
      lastFailureMessage = Prelude.Nothing
    }

-- | The status of the schema.
refreshSchemasStatus_status :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe RefreshSchemasStatusTypeValue)
refreshSchemasStatus_status = Lens.lens (\RefreshSchemasStatus' {status} -> status) (\s@RefreshSchemasStatus' {} a -> s {status = a} :: RefreshSchemasStatus)

-- | The date the schema was last refreshed.
refreshSchemasStatus_lastRefreshDate :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe Prelude.UTCTime)
refreshSchemasStatus_lastRefreshDate = Lens.lens (\RefreshSchemasStatus' {lastRefreshDate} -> lastRefreshDate) (\s@RefreshSchemasStatus' {} a -> s {lastRefreshDate = a} :: RefreshSchemasStatus) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the replication instance.
refreshSchemasStatus_replicationInstanceArn :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe Prelude.Text)
refreshSchemasStatus_replicationInstanceArn = Lens.lens (\RefreshSchemasStatus' {replicationInstanceArn} -> replicationInstanceArn) (\s@RefreshSchemasStatus' {} a -> s {replicationInstanceArn = a} :: RefreshSchemasStatus)

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
refreshSchemasStatus_endpointArn :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe Prelude.Text)
refreshSchemasStatus_endpointArn = Lens.lens (\RefreshSchemasStatus' {endpointArn} -> endpointArn) (\s@RefreshSchemasStatus' {} a -> s {endpointArn = a} :: RefreshSchemasStatus)

-- | The last failure message for the schema.
refreshSchemasStatus_lastFailureMessage :: Lens.Lens' RefreshSchemasStatus (Prelude.Maybe Prelude.Text)
refreshSchemasStatus_lastFailureMessage = Lens.lens (\RefreshSchemasStatus' {lastFailureMessage} -> lastFailureMessage) (\s@RefreshSchemasStatus' {} a -> s {lastFailureMessage = a} :: RefreshSchemasStatus)

instance Core.FromJSON RefreshSchemasStatus where
  parseJSON =
    Core.withObject
      "RefreshSchemasStatus"
      ( \x ->
          RefreshSchemasStatus'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "LastRefreshDate")
            Prelude.<*> (x Core..:? "ReplicationInstanceArn")
            Prelude.<*> (x Core..:? "EndpointArn")
            Prelude.<*> (x Core..:? "LastFailureMessage")
      )

instance Prelude.Hashable RefreshSchemasStatus where
  hashWithSalt salt' RefreshSchemasStatus' {..} =
    salt' `Prelude.hashWithSalt` lastFailureMessage
      `Prelude.hashWithSalt` endpointArn
      `Prelude.hashWithSalt` replicationInstanceArn
      `Prelude.hashWithSalt` lastRefreshDate
      `Prelude.hashWithSalt` status

instance Prelude.NFData RefreshSchemasStatus where
  rnf RefreshSchemasStatus' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastFailureMessage
      `Prelude.seq` Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf replicationInstanceArn
      `Prelude.seq` Prelude.rnf lastRefreshDate
