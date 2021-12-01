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
-- Module      : Amazonka.LookoutEquipment.Types.IngestionInputConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.IngestionInputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutEquipment.Types.IngestionS3InputConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration information for the input data for the data
-- ingestion job, including input data S3 location.
--
-- /See:/ 'newIngestionInputConfiguration' smart constructor.
data IngestionInputConfiguration = IngestionInputConfiguration'
  { -- | The location information for the S3 bucket used for input data for the
    -- data ingestion.
    s3InputConfiguration :: IngestionS3InputConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngestionInputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3InputConfiguration', 'ingestionInputConfiguration_s3InputConfiguration' - The location information for the S3 bucket used for input data for the
-- data ingestion.
newIngestionInputConfiguration ::
  -- | 's3InputConfiguration'
  IngestionS3InputConfiguration ->
  IngestionInputConfiguration
newIngestionInputConfiguration pS3InputConfiguration_ =
  IngestionInputConfiguration'
    { s3InputConfiguration =
        pS3InputConfiguration_
    }

-- | The location information for the S3 bucket used for input data for the
-- data ingestion.
ingestionInputConfiguration_s3InputConfiguration :: Lens.Lens' IngestionInputConfiguration IngestionS3InputConfiguration
ingestionInputConfiguration_s3InputConfiguration = Lens.lens (\IngestionInputConfiguration' {s3InputConfiguration} -> s3InputConfiguration) (\s@IngestionInputConfiguration' {} a -> s {s3InputConfiguration = a} :: IngestionInputConfiguration)

instance Core.FromJSON IngestionInputConfiguration where
  parseJSON =
    Core.withObject
      "IngestionInputConfiguration"
      ( \x ->
          IngestionInputConfiguration'
            Prelude.<$> (x Core..: "S3InputConfiguration")
      )

instance Prelude.Hashable IngestionInputConfiguration where
  hashWithSalt salt' IngestionInputConfiguration' {..} =
    salt' `Prelude.hashWithSalt` s3InputConfiguration

instance Prelude.NFData IngestionInputConfiguration where
  rnf IngestionInputConfiguration' {..} =
    Prelude.rnf s3InputConfiguration

instance Core.ToJSON IngestionInputConfiguration where
  toJSON IngestionInputConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "S3InputConfiguration"
                  Core..= s3InputConfiguration
              )
          ]
      )
