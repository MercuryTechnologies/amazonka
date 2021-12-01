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
-- Module      : Amazonka.AppFlow.Types.RedshiftDestinationProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.RedshiftDestinationProperties where

import Amazonka.AppFlow.Types.ErrorHandlingConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Amazon Redshift is being used as a
-- destination.
--
-- /See:/ 'newRedshiftDestinationProperties' smart constructor.
data RedshiftDestinationProperties = RedshiftDestinationProperties'
  { -- | The object key for the bucket in which Amazon AppFlow places the
    -- destination files.
    bucketPrefix :: Prelude.Maybe Prelude.Text,
    -- | The settings that determine how Amazon AppFlow handles an error when
    -- placing data in the Amazon Redshift destination. For example, this
    -- setting would determine if the flow should fail after one insertion
    -- error, or continue and attempt to insert every record regardless of the
    -- initial failure. @ErrorHandlingConfig@ is a part of the destination
    -- connector details.
    errorHandlingConfig :: Prelude.Maybe ErrorHandlingConfig,
    -- | The object specified in the Amazon Redshift flow destination.
    object' :: Prelude.Text,
    -- | The intermediate bucket that Amazon AppFlow uses when moving data into
    -- Amazon Redshift.
    intermediateBucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketPrefix', 'redshiftDestinationProperties_bucketPrefix' - The object key for the bucket in which Amazon AppFlow places the
-- destination files.
--
-- 'errorHandlingConfig', 'redshiftDestinationProperties_errorHandlingConfig' - The settings that determine how Amazon AppFlow handles an error when
-- placing data in the Amazon Redshift destination. For example, this
-- setting would determine if the flow should fail after one insertion
-- error, or continue and attempt to insert every record regardless of the
-- initial failure. @ErrorHandlingConfig@ is a part of the destination
-- connector details.
--
-- 'object'', 'redshiftDestinationProperties_object' - The object specified in the Amazon Redshift flow destination.
--
-- 'intermediateBucketName', 'redshiftDestinationProperties_intermediateBucketName' - The intermediate bucket that Amazon AppFlow uses when moving data into
-- Amazon Redshift.
newRedshiftDestinationProperties ::
  -- | 'object''
  Prelude.Text ->
  -- | 'intermediateBucketName'
  Prelude.Text ->
  RedshiftDestinationProperties
newRedshiftDestinationProperties
  pObject_
  pIntermediateBucketName_ =
    RedshiftDestinationProperties'
      { bucketPrefix =
          Prelude.Nothing,
        errorHandlingConfig = Prelude.Nothing,
        object' = pObject_,
        intermediateBucketName =
          pIntermediateBucketName_
      }

-- | The object key for the bucket in which Amazon AppFlow places the
-- destination files.
redshiftDestinationProperties_bucketPrefix :: Lens.Lens' RedshiftDestinationProperties (Prelude.Maybe Prelude.Text)
redshiftDestinationProperties_bucketPrefix = Lens.lens (\RedshiftDestinationProperties' {bucketPrefix} -> bucketPrefix) (\s@RedshiftDestinationProperties' {} a -> s {bucketPrefix = a} :: RedshiftDestinationProperties)

-- | The settings that determine how Amazon AppFlow handles an error when
-- placing data in the Amazon Redshift destination. For example, this
-- setting would determine if the flow should fail after one insertion
-- error, or continue and attempt to insert every record regardless of the
-- initial failure. @ErrorHandlingConfig@ is a part of the destination
-- connector details.
redshiftDestinationProperties_errorHandlingConfig :: Lens.Lens' RedshiftDestinationProperties (Prelude.Maybe ErrorHandlingConfig)
redshiftDestinationProperties_errorHandlingConfig = Lens.lens (\RedshiftDestinationProperties' {errorHandlingConfig} -> errorHandlingConfig) (\s@RedshiftDestinationProperties' {} a -> s {errorHandlingConfig = a} :: RedshiftDestinationProperties)

-- | The object specified in the Amazon Redshift flow destination.
redshiftDestinationProperties_object :: Lens.Lens' RedshiftDestinationProperties Prelude.Text
redshiftDestinationProperties_object = Lens.lens (\RedshiftDestinationProperties' {object'} -> object') (\s@RedshiftDestinationProperties' {} a -> s {object' = a} :: RedshiftDestinationProperties)

-- | The intermediate bucket that Amazon AppFlow uses when moving data into
-- Amazon Redshift.
redshiftDestinationProperties_intermediateBucketName :: Lens.Lens' RedshiftDestinationProperties Prelude.Text
redshiftDestinationProperties_intermediateBucketName = Lens.lens (\RedshiftDestinationProperties' {intermediateBucketName} -> intermediateBucketName) (\s@RedshiftDestinationProperties' {} a -> s {intermediateBucketName = a} :: RedshiftDestinationProperties)

instance Core.FromJSON RedshiftDestinationProperties where
  parseJSON =
    Core.withObject
      "RedshiftDestinationProperties"
      ( \x ->
          RedshiftDestinationProperties'
            Prelude.<$> (x Core..:? "bucketPrefix")
            Prelude.<*> (x Core..:? "errorHandlingConfig")
            Prelude.<*> (x Core..: "object")
            Prelude.<*> (x Core..: "intermediateBucketName")
      )

instance
  Prelude.Hashable
    RedshiftDestinationProperties
  where
  hashWithSalt salt' RedshiftDestinationProperties' {..} =
    salt' `Prelude.hashWithSalt` intermediateBucketName
      `Prelude.hashWithSalt` object'
      `Prelude.hashWithSalt` errorHandlingConfig
      `Prelude.hashWithSalt` bucketPrefix

instance Prelude.NFData RedshiftDestinationProperties where
  rnf RedshiftDestinationProperties' {..} =
    Prelude.rnf bucketPrefix
      `Prelude.seq` Prelude.rnf intermediateBucketName
      `Prelude.seq` Prelude.rnf object'
      `Prelude.seq` Prelude.rnf errorHandlingConfig

instance Core.ToJSON RedshiftDestinationProperties where
  toJSON RedshiftDestinationProperties' {..} =
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
