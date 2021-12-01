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
-- Module      : Amazonka.Firehose.Types.DynamicPartitioningConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.DynamicPartitioningConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.Firehose.Types.RetryOptions
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the dynamic partitioning mechanism that creates
-- smaller data sets from the streaming data by partitioning it based on
-- partition keys. Currently, dynamic partitioning is only supported for
-- Amazon S3 destinations. For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/dynamic-partitioning.html>
--
-- /See:/ 'newDynamicPartitioningConfiguration' smart constructor.
data DynamicPartitioningConfiguration = DynamicPartitioningConfiguration'
  { -- | Specifies that the dynamic partitioning is enabled for this Kinesis Data
    -- Firehose delivery stream.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The retry behavior in case Kinesis Data Firehose is unable to deliver
    -- data to an Amazon S3 prefix.
    retryOptions :: Prelude.Maybe RetryOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynamicPartitioningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'dynamicPartitioningConfiguration_enabled' - Specifies that the dynamic partitioning is enabled for this Kinesis Data
-- Firehose delivery stream.
--
-- 'retryOptions', 'dynamicPartitioningConfiguration_retryOptions' - The retry behavior in case Kinesis Data Firehose is unable to deliver
-- data to an Amazon S3 prefix.
newDynamicPartitioningConfiguration ::
  DynamicPartitioningConfiguration
newDynamicPartitioningConfiguration =
  DynamicPartitioningConfiguration'
    { enabled =
        Prelude.Nothing,
      retryOptions = Prelude.Nothing
    }

-- | Specifies that the dynamic partitioning is enabled for this Kinesis Data
-- Firehose delivery stream.
dynamicPartitioningConfiguration_enabled :: Lens.Lens' DynamicPartitioningConfiguration (Prelude.Maybe Prelude.Bool)
dynamicPartitioningConfiguration_enabled = Lens.lens (\DynamicPartitioningConfiguration' {enabled} -> enabled) (\s@DynamicPartitioningConfiguration' {} a -> s {enabled = a} :: DynamicPartitioningConfiguration)

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- data to an Amazon S3 prefix.
dynamicPartitioningConfiguration_retryOptions :: Lens.Lens' DynamicPartitioningConfiguration (Prelude.Maybe RetryOptions)
dynamicPartitioningConfiguration_retryOptions = Lens.lens (\DynamicPartitioningConfiguration' {retryOptions} -> retryOptions) (\s@DynamicPartitioningConfiguration' {} a -> s {retryOptions = a} :: DynamicPartitioningConfiguration)

instance
  Core.FromJSON
    DynamicPartitioningConfiguration
  where
  parseJSON =
    Core.withObject
      "DynamicPartitioningConfiguration"
      ( \x ->
          DynamicPartitioningConfiguration'
            Prelude.<$> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "RetryOptions")
      )

instance
  Prelude.Hashable
    DynamicPartitioningConfiguration
  where
  hashWithSalt
    salt'
    DynamicPartitioningConfiguration' {..} =
      salt' `Prelude.hashWithSalt` retryOptions
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    DynamicPartitioningConfiguration
  where
  rnf DynamicPartitioningConfiguration' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf retryOptions

instance Core.ToJSON DynamicPartitioningConfiguration where
  toJSON DynamicPartitioningConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Enabled" Core..=) Prelude.<$> enabled,
            ("RetryOptions" Core..=) Prelude.<$> retryOptions
          ]
      )
