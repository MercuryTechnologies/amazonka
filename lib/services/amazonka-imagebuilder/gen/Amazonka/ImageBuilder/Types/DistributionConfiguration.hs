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
-- Module      : Amazonka.ImageBuilder.Types.DistributionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.DistributionConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types.Distribution
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A distribution configuration.
--
-- /See:/ 'newDistributionConfiguration' smart constructor.
data DistributionConfiguration = DistributionConfiguration'
  { -- | The Amazon Resource Name (ARN) of the distribution configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date on which this distribution configuration was last updated.
    dateUpdated :: Prelude.Maybe Prelude.Text,
    -- | The date on which this distribution configuration was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The name of the distribution configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the distribution configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The distribution objects that apply Region-specific settings for the
    -- deployment of the image to targeted Regions.
    distributions :: Prelude.Maybe [Distribution],
    -- | The tags of the distribution configuration.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The maximum duration in minutes for this distribution configuration.
    timeoutMinutes :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DistributionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'distributionConfiguration_arn' - The Amazon Resource Name (ARN) of the distribution configuration.
--
-- 'dateUpdated', 'distributionConfiguration_dateUpdated' - The date on which this distribution configuration was last updated.
--
-- 'dateCreated', 'distributionConfiguration_dateCreated' - The date on which this distribution configuration was created.
--
-- 'name', 'distributionConfiguration_name' - The name of the distribution configuration.
--
-- 'description', 'distributionConfiguration_description' - The description of the distribution configuration.
--
-- 'distributions', 'distributionConfiguration_distributions' - The distribution objects that apply Region-specific settings for the
-- deployment of the image to targeted Regions.
--
-- 'tags', 'distributionConfiguration_tags' - The tags of the distribution configuration.
--
-- 'timeoutMinutes', 'distributionConfiguration_timeoutMinutes' - The maximum duration in minutes for this distribution configuration.
newDistributionConfiguration ::
  -- | 'timeoutMinutes'
  Prelude.Natural ->
  DistributionConfiguration
newDistributionConfiguration pTimeoutMinutes_ =
  DistributionConfiguration'
    { arn = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      distributions = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeoutMinutes = pTimeoutMinutes_
    }

-- | The Amazon Resource Name (ARN) of the distribution configuration.
distributionConfiguration_arn :: Lens.Lens' DistributionConfiguration (Prelude.Maybe Prelude.Text)
distributionConfiguration_arn = Lens.lens (\DistributionConfiguration' {arn} -> arn) (\s@DistributionConfiguration' {} a -> s {arn = a} :: DistributionConfiguration)

-- | The date on which this distribution configuration was last updated.
distributionConfiguration_dateUpdated :: Lens.Lens' DistributionConfiguration (Prelude.Maybe Prelude.Text)
distributionConfiguration_dateUpdated = Lens.lens (\DistributionConfiguration' {dateUpdated} -> dateUpdated) (\s@DistributionConfiguration' {} a -> s {dateUpdated = a} :: DistributionConfiguration)

-- | The date on which this distribution configuration was created.
distributionConfiguration_dateCreated :: Lens.Lens' DistributionConfiguration (Prelude.Maybe Prelude.Text)
distributionConfiguration_dateCreated = Lens.lens (\DistributionConfiguration' {dateCreated} -> dateCreated) (\s@DistributionConfiguration' {} a -> s {dateCreated = a} :: DistributionConfiguration)

-- | The name of the distribution configuration.
distributionConfiguration_name :: Lens.Lens' DistributionConfiguration (Prelude.Maybe Prelude.Text)
distributionConfiguration_name = Lens.lens (\DistributionConfiguration' {name} -> name) (\s@DistributionConfiguration' {} a -> s {name = a} :: DistributionConfiguration)

-- | The description of the distribution configuration.
distributionConfiguration_description :: Lens.Lens' DistributionConfiguration (Prelude.Maybe Prelude.Text)
distributionConfiguration_description = Lens.lens (\DistributionConfiguration' {description} -> description) (\s@DistributionConfiguration' {} a -> s {description = a} :: DistributionConfiguration)

-- | The distribution objects that apply Region-specific settings for the
-- deployment of the image to targeted Regions.
distributionConfiguration_distributions :: Lens.Lens' DistributionConfiguration (Prelude.Maybe [Distribution])
distributionConfiguration_distributions = Lens.lens (\DistributionConfiguration' {distributions} -> distributions) (\s@DistributionConfiguration' {} a -> s {distributions = a} :: DistributionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The tags of the distribution configuration.
distributionConfiguration_tags :: Lens.Lens' DistributionConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
distributionConfiguration_tags = Lens.lens (\DistributionConfiguration' {tags} -> tags) (\s@DistributionConfiguration' {} a -> s {tags = a} :: DistributionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The maximum duration in minutes for this distribution configuration.
distributionConfiguration_timeoutMinutes :: Lens.Lens' DistributionConfiguration Prelude.Natural
distributionConfiguration_timeoutMinutes = Lens.lens (\DistributionConfiguration' {timeoutMinutes} -> timeoutMinutes) (\s@DistributionConfiguration' {} a -> s {timeoutMinutes = a} :: DistributionConfiguration)

instance Core.FromJSON DistributionConfiguration where
  parseJSON =
    Core.withObject
      "DistributionConfiguration"
      ( \x ->
          DistributionConfiguration'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "dateUpdated")
            Prelude.<*> (x Core..:? "dateCreated")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "distributions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "timeoutMinutes")
      )

instance Prelude.Hashable DistributionConfiguration where
  hashWithSalt salt' DistributionConfiguration' {..} =
    salt' `Prelude.hashWithSalt` timeoutMinutes
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` distributions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` dateUpdated
      `Prelude.hashWithSalt` arn

instance Prelude.NFData DistributionConfiguration where
  rnf DistributionConfiguration' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf timeoutMinutes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf distributions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateUpdated
