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
-- Module      : Amazonka.Redshift.Types.PartnerIntegrationInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.PartnerIntegrationInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.PartnerIntegrationStatus

-- | Describes a partner integration.
--
-- /See:/ 'newPartnerIntegrationInfo' smart constructor.
data PartnerIntegrationInfo = PartnerIntegrationInfo'
  { -- | The partner integration status.
    status :: Prelude.Maybe PartnerIntegrationStatus,
    -- | The date (UTC) that the partner integration was created.
    createdAt :: Prelude.Maybe Core.ISO8601,
    -- | The name of the partner.
    partnerName :: Prelude.Maybe Prelude.Text,
    -- | The status message provided by the partner.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the database that receives data from a partner.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The date (UTC) that the partner integration status was last updated by
    -- the partner.
    updatedAt :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartnerIntegrationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'partnerIntegrationInfo_status' - The partner integration status.
--
-- 'createdAt', 'partnerIntegrationInfo_createdAt' - The date (UTC) that the partner integration was created.
--
-- 'partnerName', 'partnerIntegrationInfo_partnerName' - The name of the partner.
--
-- 'statusMessage', 'partnerIntegrationInfo_statusMessage' - The status message provided by the partner.
--
-- 'databaseName', 'partnerIntegrationInfo_databaseName' - The name of the database that receives data from a partner.
--
-- 'updatedAt', 'partnerIntegrationInfo_updatedAt' - The date (UTC) that the partner integration status was last updated by
-- the partner.
newPartnerIntegrationInfo ::
  PartnerIntegrationInfo
newPartnerIntegrationInfo =
  PartnerIntegrationInfo'
    { status = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      partnerName = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The partner integration status.
partnerIntegrationInfo_status :: Lens.Lens' PartnerIntegrationInfo (Prelude.Maybe PartnerIntegrationStatus)
partnerIntegrationInfo_status = Lens.lens (\PartnerIntegrationInfo' {status} -> status) (\s@PartnerIntegrationInfo' {} a -> s {status = a} :: PartnerIntegrationInfo)

-- | The date (UTC) that the partner integration was created.
partnerIntegrationInfo_createdAt :: Lens.Lens' PartnerIntegrationInfo (Prelude.Maybe Prelude.UTCTime)
partnerIntegrationInfo_createdAt = Lens.lens (\PartnerIntegrationInfo' {createdAt} -> createdAt) (\s@PartnerIntegrationInfo' {} a -> s {createdAt = a} :: PartnerIntegrationInfo) Prelude.. Lens.mapping Core._Time

-- | The name of the partner.
partnerIntegrationInfo_partnerName :: Lens.Lens' PartnerIntegrationInfo (Prelude.Maybe Prelude.Text)
partnerIntegrationInfo_partnerName = Lens.lens (\PartnerIntegrationInfo' {partnerName} -> partnerName) (\s@PartnerIntegrationInfo' {} a -> s {partnerName = a} :: PartnerIntegrationInfo)

-- | The status message provided by the partner.
partnerIntegrationInfo_statusMessage :: Lens.Lens' PartnerIntegrationInfo (Prelude.Maybe Prelude.Text)
partnerIntegrationInfo_statusMessage = Lens.lens (\PartnerIntegrationInfo' {statusMessage} -> statusMessage) (\s@PartnerIntegrationInfo' {} a -> s {statusMessage = a} :: PartnerIntegrationInfo)

-- | The name of the database that receives data from a partner.
partnerIntegrationInfo_databaseName :: Lens.Lens' PartnerIntegrationInfo (Prelude.Maybe Prelude.Text)
partnerIntegrationInfo_databaseName = Lens.lens (\PartnerIntegrationInfo' {databaseName} -> databaseName) (\s@PartnerIntegrationInfo' {} a -> s {databaseName = a} :: PartnerIntegrationInfo)

-- | The date (UTC) that the partner integration status was last updated by
-- the partner.
partnerIntegrationInfo_updatedAt :: Lens.Lens' PartnerIntegrationInfo (Prelude.Maybe Prelude.UTCTime)
partnerIntegrationInfo_updatedAt = Lens.lens (\PartnerIntegrationInfo' {updatedAt} -> updatedAt) (\s@PartnerIntegrationInfo' {} a -> s {updatedAt = a} :: PartnerIntegrationInfo) Prelude.. Lens.mapping Core._Time

instance Core.FromXML PartnerIntegrationInfo where
  parseXML x =
    PartnerIntegrationInfo'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "CreatedAt")
      Prelude.<*> (x Core..@? "PartnerName")
      Prelude.<*> (x Core..@? "StatusMessage")
      Prelude.<*> (x Core..@? "DatabaseName")
      Prelude.<*> (x Core..@? "UpdatedAt")

instance Prelude.Hashable PartnerIntegrationInfo where
  hashWithSalt salt' PartnerIntegrationInfo' {..} =
    salt' `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` partnerName
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` status

instance Prelude.NFData PartnerIntegrationInfo where
  rnf PartnerIntegrationInfo' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf partnerName
      `Prelude.seq` Prelude.rnf createdAt
