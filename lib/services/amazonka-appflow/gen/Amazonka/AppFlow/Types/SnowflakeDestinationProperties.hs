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
-- Module      : Amazonka.AppFlow.Types.SnowflakeDestinationProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SnowflakeDestinationProperties where

import Amazonka.AppFlow.Types.ErrorHandlingConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Snowflake is being used as a
-- destination.
--
-- /See:/ 'newSnowflakeDestinationProperties' smart constructor.
data SnowflakeDestinationProperties = SnowflakeDestinationProperties'
  { -- | The object key for the destination bucket in which Amazon AppFlow places
    -- the files.
    bucketPrefix :: Prelude.Maybe Prelude.Text,
    -- | The settings that determine how Amazon AppFlow handles an error when
    -- placing data in the Snowflake destination. For example, this setting
    -- would determine if the flow should fail after one insertion error, or
    -- continue and attempt to insert every record regardless of the initial
    -- failure. @ErrorHandlingConfig@ is a part of the destination connector
    -- details.
    errorHandlingConfig :: Prelude.Maybe ErrorHandlingConfig,
    -- | The object specified in the Snowflake flow destination.
    object' :: Prelude.Text,
    -- | The intermediate bucket that Amazon AppFlow uses when moving data into
    -- Snowflake.
    intermediateBucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnowflakeDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketPrefix', 'snowflakeDestinationProperties_bucketPrefix' - The object key for the destination bucket in which Amazon AppFlow places
-- the files.
--
-- 'errorHandlingConfig', 'snowflakeDestinationProperties_errorHandlingConfig' - The settings that determine how Amazon AppFlow handles an error when
-- placing data in the Snowflake destination. For example, this setting
-- would determine if the flow should fail after one insertion error, or
-- continue and attempt to insert every record regardless of the initial
-- failure. @ErrorHandlingConfig@ is a part of the destination connector
-- details.
--
-- 'object'', 'snowflakeDestinationProperties_object' - The object specified in the Snowflake flow destination.
--
-- 'intermediateBucketName', 'snowflakeDestinationProperties_intermediateBucketName' - The intermediate bucket that Amazon AppFlow uses when moving data into
-- Snowflake.
newSnowflakeDestinationProperties ::
  -- | 'object''
  Prelude.Text ->
  -- | 'intermediateBucketName'
  Prelude.Text ->
  SnowflakeDestinationProperties
newSnowflakeDestinationProperties
  pObject_
  pIntermediateBucketName_ =
    SnowflakeDestinationProperties'
      { bucketPrefix =
          Prelude.Nothing,
        errorHandlingConfig = Prelude.Nothing,
        object' = pObject_,
        intermediateBucketName =
          pIntermediateBucketName_
      }

-- | The object key for the destination bucket in which Amazon AppFlow places
-- the files.
snowflakeDestinationProperties_bucketPrefix :: Lens.Lens' SnowflakeDestinationProperties (Prelude.Maybe Prelude.Text)
snowflakeDestinationProperties_bucketPrefix = Lens.lens (\SnowflakeDestinationProperties' {bucketPrefix} -> bucketPrefix) (\s@SnowflakeDestinationProperties' {} a -> s {bucketPrefix = a} :: SnowflakeDestinationProperties)

-- | The settings that determine how Amazon AppFlow handles an error when
-- placing data in the Snowflake destination. For example, this setting
-- would determine if the flow should fail after one insertion error, or
-- continue and attempt to insert every record regardless of the initial
-- failure. @ErrorHandlingConfig@ is a part of the destination connector
-- details.
snowflakeDestinationProperties_errorHandlingConfig :: Lens.Lens' SnowflakeDestinationProperties (Prelude.Maybe ErrorHandlingConfig)
snowflakeDestinationProperties_errorHandlingConfig = Lens.lens (\SnowflakeDestinationProperties' {errorHandlingConfig} -> errorHandlingConfig) (\s@SnowflakeDestinationProperties' {} a -> s {errorHandlingConfig = a} :: SnowflakeDestinationProperties)

-- | The object specified in the Snowflake flow destination.
snowflakeDestinationProperties_object :: Lens.Lens' SnowflakeDestinationProperties Prelude.Text
snowflakeDestinationProperties_object = Lens.lens (\SnowflakeDestinationProperties' {object'} -> object') (\s@SnowflakeDestinationProperties' {} a -> s {object' = a} :: SnowflakeDestinationProperties)

-- | The intermediate bucket that Amazon AppFlow uses when moving data into
-- Snowflake.
snowflakeDestinationProperties_intermediateBucketName :: Lens.Lens' SnowflakeDestinationProperties Prelude.Text
snowflakeDestinationProperties_intermediateBucketName = Lens.lens (\SnowflakeDestinationProperties' {intermediateBucketName} -> intermediateBucketName) (\s@SnowflakeDestinationProperties' {} a -> s {intermediateBucketName = a} :: SnowflakeDestinationProperties)

instance Core.FromJSON SnowflakeDestinationProperties where
  parseJSON =
    Core.withObject
      "SnowflakeDestinationProperties"
      ( \x ->
          SnowflakeDestinationProperties'
            Prelude.<$> (x Core..:? "bucketPrefix")
            Prelude.<*> (x Core..:? "errorHandlingConfig")
            Prelude.<*> (x Core..: "object")
            Prelude.<*> (x Core..: "intermediateBucketName")
      )

instance
  Prelude.Hashable
    SnowflakeDestinationProperties
  where
  hashWithSalt
    salt'
    SnowflakeDestinationProperties' {..} =
      salt' `Prelude.hashWithSalt` intermediateBucketName
        `Prelude.hashWithSalt` object'
        `Prelude.hashWithSalt` errorHandlingConfig
        `Prelude.hashWithSalt` bucketPrefix

instance
  Prelude.NFData
    SnowflakeDestinationProperties
  where
  rnf SnowflakeDestinationProperties' {..} =
    Prelude.rnf bucketPrefix
      `Prelude.seq` Prelude.rnf intermediateBucketName
      `Prelude.seq` Prelude.rnf object'
      `Prelude.seq` Prelude.rnf errorHandlingConfig

instance Core.ToJSON SnowflakeDestinationProperties where
  toJSON SnowflakeDestinationProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("bucketPrefix" Core..=) Prelude.<$> bucketPrefix,
            ("errorHandlingConfig" Core..=)
              Prelude.<$> errorHandlingConfig,
            Prelude.Just ("object" Core..= object'),
            Prelude.Just
              ( "intermediateBucketName"
                  Core..= intermediateBucketName
              )
          ]
      )
