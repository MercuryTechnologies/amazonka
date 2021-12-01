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
-- Module      : Amazonka.IoTSiteWise.Types.AssociatedAssetsSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssociatedAssetsSummary where

import qualified Amazonka.Core as Core
import Amazonka.IoTSiteWise.Types.AssetHierarchy
import Amazonka.IoTSiteWise.Types.AssetStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of an associated asset.
--
-- /See:/ 'newAssociatedAssetsSummary' smart constructor.
data AssociatedAssetsSummary = AssociatedAssetsSummary'
  { -- | The ID of the asset.
    id :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the asset, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
    arn :: Prelude.Text,
    -- | The name of the asset.
    name :: Prelude.Text,
    -- | The ID of the asset model used to create the asset.
    assetModelId :: Prelude.Text,
    -- | The date the asset was created, in Unix epoch time.
    creationDate :: Core.POSIX,
    -- | The date the asset was last updated, in Unix epoch time.
    lastUpdateDate :: Core.POSIX,
    -- | The current status of the asset.
    status :: AssetStatus,
    -- | A list of asset hierarchies that each contain a @hierarchyId@. A
    -- hierarchy specifies allowed parent\/child asset relationships.
    hierarchies :: [AssetHierarchy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatedAssetsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'associatedAssetsSummary_id' - The ID of the asset.
--
-- 'arn', 'associatedAssetsSummary_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
--
-- 'name', 'associatedAssetsSummary_name' - The name of the asset.
--
-- 'assetModelId', 'associatedAssetsSummary_assetModelId' - The ID of the asset model used to create the asset.
--
-- 'creationDate', 'associatedAssetsSummary_creationDate' - The date the asset was created, in Unix epoch time.
--
-- 'lastUpdateDate', 'associatedAssetsSummary_lastUpdateDate' - The date the asset was last updated, in Unix epoch time.
--
-- 'status', 'associatedAssetsSummary_status' - The current status of the asset.
--
-- 'hierarchies', 'associatedAssetsSummary_hierarchies' - A list of asset hierarchies that each contain a @hierarchyId@. A
-- hierarchy specifies allowed parent\/child asset relationships.
newAssociatedAssetsSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'assetModelId'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  -- | 'lastUpdateDate'
  Prelude.UTCTime ->
  -- | 'status'
  AssetStatus ->
  AssociatedAssetsSummary
newAssociatedAssetsSummary
  pId_
  pArn_
  pName_
  pAssetModelId_
  pCreationDate_
  pLastUpdateDate_
  pStatus_ =
    AssociatedAssetsSummary'
      { id = pId_,
        arn = pArn_,
        name = pName_,
        assetModelId = pAssetModelId_,
        creationDate = Core._Time Lens.# pCreationDate_,
        lastUpdateDate =
          Core._Time Lens.# pLastUpdateDate_,
        status = pStatus_,
        hierarchies = Prelude.mempty
      }

-- | The ID of the asset.
associatedAssetsSummary_id :: Lens.Lens' AssociatedAssetsSummary Prelude.Text
associatedAssetsSummary_id = Lens.lens (\AssociatedAssetsSummary' {id} -> id) (\s@AssociatedAssetsSummary' {} a -> s {id = a} :: AssociatedAssetsSummary)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
associatedAssetsSummary_arn :: Lens.Lens' AssociatedAssetsSummary Prelude.Text
associatedAssetsSummary_arn = Lens.lens (\AssociatedAssetsSummary' {arn} -> arn) (\s@AssociatedAssetsSummary' {} a -> s {arn = a} :: AssociatedAssetsSummary)

-- | The name of the asset.
associatedAssetsSummary_name :: Lens.Lens' AssociatedAssetsSummary Prelude.Text
associatedAssetsSummary_name = Lens.lens (\AssociatedAssetsSummary' {name} -> name) (\s@AssociatedAssetsSummary' {} a -> s {name = a} :: AssociatedAssetsSummary)

-- | The ID of the asset model used to create the asset.
associatedAssetsSummary_assetModelId :: Lens.Lens' AssociatedAssetsSummary Prelude.Text
associatedAssetsSummary_assetModelId = Lens.lens (\AssociatedAssetsSummary' {assetModelId} -> assetModelId) (\s@AssociatedAssetsSummary' {} a -> s {assetModelId = a} :: AssociatedAssetsSummary)

-- | The date the asset was created, in Unix epoch time.
associatedAssetsSummary_creationDate :: Lens.Lens' AssociatedAssetsSummary Prelude.UTCTime
associatedAssetsSummary_creationDate = Lens.lens (\AssociatedAssetsSummary' {creationDate} -> creationDate) (\s@AssociatedAssetsSummary' {} a -> s {creationDate = a} :: AssociatedAssetsSummary) Prelude.. Core._Time

-- | The date the asset was last updated, in Unix epoch time.
associatedAssetsSummary_lastUpdateDate :: Lens.Lens' AssociatedAssetsSummary Prelude.UTCTime
associatedAssetsSummary_lastUpdateDate = Lens.lens (\AssociatedAssetsSummary' {lastUpdateDate} -> lastUpdateDate) (\s@AssociatedAssetsSummary' {} a -> s {lastUpdateDate = a} :: AssociatedAssetsSummary) Prelude.. Core._Time

-- | The current status of the asset.
associatedAssetsSummary_status :: Lens.Lens' AssociatedAssetsSummary AssetStatus
associatedAssetsSummary_status = Lens.lens (\AssociatedAssetsSummary' {status} -> status) (\s@AssociatedAssetsSummary' {} a -> s {status = a} :: AssociatedAssetsSummary)

-- | A list of asset hierarchies that each contain a @hierarchyId@. A
-- hierarchy specifies allowed parent\/child asset relationships.
associatedAssetsSummary_hierarchies :: Lens.Lens' AssociatedAssetsSummary [AssetHierarchy]
associatedAssetsSummary_hierarchies = Lens.lens (\AssociatedAssetsSummary' {hierarchies} -> hierarchies) (\s@AssociatedAssetsSummary' {} a -> s {hierarchies = a} :: AssociatedAssetsSummary) Prelude.. Lens.coerced

instance Core.FromJSON AssociatedAssetsSummary where
  parseJSON =
    Core.withObject
      "AssociatedAssetsSummary"
      ( \x ->
          AssociatedAssetsSummary'
            Prelude.<$> (x Core..: "id")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "assetModelId")
            Prelude.<*> (x Core..: "creationDate")
            Prelude.<*> (x Core..: "lastUpdateDate")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..:? "hierarchies" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AssociatedAssetsSummary where
  hashWithSalt salt' AssociatedAssetsSummary' {..} =
    salt' `Prelude.hashWithSalt` hierarchies
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdateDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` assetModelId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id

instance Prelude.NFData AssociatedAssetsSummary where
  rnf AssociatedAssetsSummary' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf hierarchies
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdateDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf assetModelId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
