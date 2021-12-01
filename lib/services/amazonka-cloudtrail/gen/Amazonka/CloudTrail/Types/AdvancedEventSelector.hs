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
-- Module      : Amazonka.CloudTrail.Types.AdvancedEventSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.AdvancedEventSelector where

import Amazonka.CloudTrail.Types.AdvancedFieldSelector
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Advanced event selectors let you create fine-grained selectors for the
-- following CloudTrail event record ﬁelds. They help you control costs by
-- logging only those events that are important to you. For more
-- information about advanced event selectors, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-data-events-with-cloudtrail.html Logging data events for trails>
-- in the /CloudTrail User Guide/.
--
-- -   @readOnly@
--
-- -   @eventSource@
--
-- -   @eventName@
--
-- -   @eventCategory@
--
-- -   @resources.type@
--
-- -   @resources.ARN@
--
-- You cannot apply both event selectors and advanced event selectors to a
-- trail.
--
-- /See:/ 'newAdvancedEventSelector' smart constructor.
data AdvancedEventSelector = AdvancedEventSelector'
  { -- | An optional, descriptive name for an advanced event selector, such as
    -- \"Log data events for only two S3 buckets\".
    name :: Prelude.Maybe Prelude.Text,
    -- | Contains all selector statements in an advanced event selector.
    fieldSelectors :: Prelude.NonEmpty AdvancedFieldSelector
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvancedEventSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'advancedEventSelector_name' - An optional, descriptive name for an advanced event selector, such as
-- \"Log data events for only two S3 buckets\".
--
-- 'fieldSelectors', 'advancedEventSelector_fieldSelectors' - Contains all selector statements in an advanced event selector.
newAdvancedEventSelector ::
  -- | 'fieldSelectors'
  Prelude.NonEmpty AdvancedFieldSelector ->
  AdvancedEventSelector
newAdvancedEventSelector pFieldSelectors_ =
  AdvancedEventSelector'
    { name = Prelude.Nothing,
      fieldSelectors =
        Lens.coerced Lens.# pFieldSelectors_
    }

-- | An optional, descriptive name for an advanced event selector, such as
-- \"Log data events for only two S3 buckets\".
advancedEventSelector_name :: Lens.Lens' AdvancedEventSelector (Prelude.Maybe Prelude.Text)
advancedEventSelector_name = Lens.lens (\AdvancedEventSelector' {name} -> name) (\s@AdvancedEventSelector' {} a -> s {name = a} :: AdvancedEventSelector)

-- | Contains all selector statements in an advanced event selector.
advancedEventSelector_fieldSelectors :: Lens.Lens' AdvancedEventSelector (Prelude.NonEmpty AdvancedFieldSelector)
advancedEventSelector_fieldSelectors = Lens.lens (\AdvancedEventSelector' {fieldSelectors} -> fieldSelectors) (\s@AdvancedEventSelector' {} a -> s {fieldSelectors = a} :: AdvancedEventSelector) Prelude.. Lens.coerced

instance Core.FromJSON AdvancedEventSelector where
  parseJSON =
    Core.withObject
      "AdvancedEventSelector"
      ( \x ->
          AdvancedEventSelector'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..: "FieldSelectors")
      )

instance Prelude.Hashable AdvancedEventSelector where
  hashWithSalt salt' AdvancedEventSelector' {..} =
    salt' `Prelude.hashWithSalt` fieldSelectors
      `Prelude.hashWithSalt` name

instance Prelude.NFData AdvancedEventSelector where
  rnf AdvancedEventSelector' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf fieldSelectors

instance Core.ToJSON AdvancedEventSelector where
  toJSON AdvancedEventSelector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            Prelude.Just
              ("FieldSelectors" Core..= fieldSelectors)
          ]
      )
