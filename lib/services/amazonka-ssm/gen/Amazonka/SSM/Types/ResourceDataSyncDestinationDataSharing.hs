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
-- Module      : Amazonka.SSM.Types.ResourceDataSyncDestinationDataSharing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ResourceDataSyncDestinationDataSharing where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Synchronize Amazon Web Services Systems Manager Inventory data from
-- multiple Amazon Web Services accounts defined in Organizations to a
-- centralized Amazon S3 bucket. Data is synchronized to individual key
-- prefixes in the central bucket. Each key prefix represents a different
-- Amazon Web Services account ID.
--
-- /See:/ 'newResourceDataSyncDestinationDataSharing' smart constructor.
data ResourceDataSyncDestinationDataSharing = ResourceDataSyncDestinationDataSharing'
  { -- | The sharing data type. Only @Organization@ is supported.
    destinationDataSharingType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDataSyncDestinationDataSharing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationDataSharingType', 'resourceDataSyncDestinationDataSharing_destinationDataSharingType' - The sharing data type. Only @Organization@ is supported.
newResourceDataSyncDestinationDataSharing ::
  ResourceDataSyncDestinationDataSharing
newResourceDataSyncDestinationDataSharing =
  ResourceDataSyncDestinationDataSharing'
    { destinationDataSharingType =
        Prelude.Nothing
    }

-- | The sharing data type. Only @Organization@ is supported.
resourceDataSyncDestinationDataSharing_destinationDataSharingType :: Lens.Lens' ResourceDataSyncDestinationDataSharing (Prelude.Maybe Prelude.Text)
resourceDataSyncDestinationDataSharing_destinationDataSharingType = Lens.lens (\ResourceDataSyncDestinationDataSharing' {destinationDataSharingType} -> destinationDataSharingType) (\s@ResourceDataSyncDestinationDataSharing' {} a -> s {destinationDataSharingType = a} :: ResourceDataSyncDestinationDataSharing)

instance
  Core.FromJSON
    ResourceDataSyncDestinationDataSharing
  where
  parseJSON =
    Core.withObject
      "ResourceDataSyncDestinationDataSharing"
      ( \x ->
          ResourceDataSyncDestinationDataSharing'
            Prelude.<$> (x Core..:? "DestinationDataSharingType")
      )

instance
  Prelude.Hashable
    ResourceDataSyncDestinationDataSharing
  where
  hashWithSalt
    salt'
    ResourceDataSyncDestinationDataSharing' {..} =
      salt'
        `Prelude.hashWithSalt` destinationDataSharingType

instance
  Prelude.NFData
    ResourceDataSyncDestinationDataSharing
  where
  rnf ResourceDataSyncDestinationDataSharing' {..} =
    Prelude.rnf destinationDataSharingType

instance
  Core.ToJSON
    ResourceDataSyncDestinationDataSharing
  where
  toJSON ResourceDataSyncDestinationDataSharing' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DestinationDataSharingType" Core..=)
              Prelude.<$> destinationDataSharingType
          ]
      )
