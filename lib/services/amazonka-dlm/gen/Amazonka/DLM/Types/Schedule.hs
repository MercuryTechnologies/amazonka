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
-- Module      : Amazonka.DLM.Types.Schedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.Schedule where

import qualified Amazonka.Core as Core
import Amazonka.DLM.Types.CreateRule
import Amazonka.DLM.Types.CrossRegionCopyRule
import Amazonka.DLM.Types.DeprecateRule
import Amazonka.DLM.Types.FastRestoreRule
import Amazonka.DLM.Types.RetainRule
import Amazonka.DLM.Types.ShareRule
import Amazonka.DLM.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a backup schedule for a snapshot or AMI lifecycle policy.
--
-- /See:/ 'newSchedule' smart constructor.
data Schedule = Schedule'
  { -- | A collection of key\/value pairs with values determined dynamically when
    -- the policy is executed. Keys may be any valid Amazon EC2 tag key. Values
    -- must be in one of the two following formats: @$(instance-id)@ or
    -- @$(timestamp)@. Variable tags are only valid for EBS Snapshot Management
    -- – Instance policies.
    variableTags :: Prelude.Maybe [Tag],
    -- | The creation rule.
    createRule :: Prelude.Maybe CreateRule,
    -- | The AMI deprecation rule for the schedule.
    deprecateRule :: Prelude.Maybe DeprecateRule,
    -- | Copy all user-defined tags on a source volume to snapshots of the volume
    -- created by this policy.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | The name of the schedule.
    name :: Prelude.Maybe Prelude.Text,
    -- | The rule for sharing snapshots with other Amazon Web Services accounts.
    shareRules :: Prelude.Maybe [ShareRule],
    -- | The tags to apply to policy-created resources. These user-defined tags
    -- are in addition to the Amazon Web Services-added lifecycle tags.
    tagsToAdd :: Prelude.Maybe [Tag],
    -- | The retention rule.
    retainRule :: Prelude.Maybe RetainRule,
    -- | The rule for cross-Region snapshot copies.
    --
    -- You can only specify cross-Region copy rules for policies that create
    -- snapshots in a Region. If the policy creates snapshots on an Outpost,
    -- then you cannot copy the snapshots to a Region or to an Outpost. If the
    -- policy creates snapshots in a Region, then snapshots can be copied to up
    -- to three Regions or Outposts.
    crossRegionCopyRules :: Prelude.Maybe [CrossRegionCopyRule],
    -- | The rule for enabling fast snapshot restore.
    fastRestoreRule :: Prelude.Maybe FastRestoreRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Schedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variableTags', 'schedule_variableTags' - A collection of key\/value pairs with values determined dynamically when
-- the policy is executed. Keys may be any valid Amazon EC2 tag key. Values
-- must be in one of the two following formats: @$(instance-id)@ or
-- @$(timestamp)@. Variable tags are only valid for EBS Snapshot Management
-- – Instance policies.
--
-- 'createRule', 'schedule_createRule' - The creation rule.
--
-- 'deprecateRule', 'schedule_deprecateRule' - The AMI deprecation rule for the schedule.
--
-- 'copyTags', 'schedule_copyTags' - Copy all user-defined tags on a source volume to snapshots of the volume
-- created by this policy.
--
-- 'name', 'schedule_name' - The name of the schedule.
--
-- 'shareRules', 'schedule_shareRules' - The rule for sharing snapshots with other Amazon Web Services accounts.
--
-- 'tagsToAdd', 'schedule_tagsToAdd' - The tags to apply to policy-created resources. These user-defined tags
-- are in addition to the Amazon Web Services-added lifecycle tags.
--
-- 'retainRule', 'schedule_retainRule' - The retention rule.
--
-- 'crossRegionCopyRules', 'schedule_crossRegionCopyRules' - The rule for cross-Region snapshot copies.
--
-- You can only specify cross-Region copy rules for policies that create
-- snapshots in a Region. If the policy creates snapshots on an Outpost,
-- then you cannot copy the snapshots to a Region or to an Outpost. If the
-- policy creates snapshots in a Region, then snapshots can be copied to up
-- to three Regions or Outposts.
--
-- 'fastRestoreRule', 'schedule_fastRestoreRule' - The rule for enabling fast snapshot restore.
newSchedule ::
  Schedule
newSchedule =
  Schedule'
    { variableTags = Prelude.Nothing,
      createRule = Prelude.Nothing,
      deprecateRule = Prelude.Nothing,
      copyTags = Prelude.Nothing,
      name = Prelude.Nothing,
      shareRules = Prelude.Nothing,
      tagsToAdd = Prelude.Nothing,
      retainRule = Prelude.Nothing,
      crossRegionCopyRules = Prelude.Nothing,
      fastRestoreRule = Prelude.Nothing
    }

-- | A collection of key\/value pairs with values determined dynamically when
-- the policy is executed. Keys may be any valid Amazon EC2 tag key. Values
-- must be in one of the two following formats: @$(instance-id)@ or
-- @$(timestamp)@. Variable tags are only valid for EBS Snapshot Management
-- – Instance policies.
schedule_variableTags :: Lens.Lens' Schedule (Prelude.Maybe [Tag])
schedule_variableTags = Lens.lens (\Schedule' {variableTags} -> variableTags) (\s@Schedule' {} a -> s {variableTags = a} :: Schedule) Prelude.. Lens.mapping Lens.coerced

-- | The creation rule.
schedule_createRule :: Lens.Lens' Schedule (Prelude.Maybe CreateRule)
schedule_createRule = Lens.lens (\Schedule' {createRule} -> createRule) (\s@Schedule' {} a -> s {createRule = a} :: Schedule)

-- | The AMI deprecation rule for the schedule.
schedule_deprecateRule :: Lens.Lens' Schedule (Prelude.Maybe DeprecateRule)
schedule_deprecateRule = Lens.lens (\Schedule' {deprecateRule} -> deprecateRule) (\s@Schedule' {} a -> s {deprecateRule = a} :: Schedule)

-- | Copy all user-defined tags on a source volume to snapshots of the volume
-- created by this policy.
schedule_copyTags :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Bool)
schedule_copyTags = Lens.lens (\Schedule' {copyTags} -> copyTags) (\s@Schedule' {} a -> s {copyTags = a} :: Schedule)

-- | The name of the schedule.
schedule_name :: Lens.Lens' Schedule (Prelude.Maybe Prelude.Text)
schedule_name = Lens.lens (\Schedule' {name} -> name) (\s@Schedule' {} a -> s {name = a} :: Schedule)

-- | The rule for sharing snapshots with other Amazon Web Services accounts.
schedule_shareRules :: Lens.Lens' Schedule (Prelude.Maybe [ShareRule])
schedule_shareRules = Lens.lens (\Schedule' {shareRules} -> shareRules) (\s@Schedule' {} a -> s {shareRules = a} :: Schedule) Prelude.. Lens.mapping Lens.coerced

-- | The tags to apply to policy-created resources. These user-defined tags
-- are in addition to the Amazon Web Services-added lifecycle tags.
schedule_tagsToAdd :: Lens.Lens' Schedule (Prelude.Maybe [Tag])
schedule_tagsToAdd = Lens.lens (\Schedule' {tagsToAdd} -> tagsToAdd) (\s@Schedule' {} a -> s {tagsToAdd = a} :: Schedule) Prelude.. Lens.mapping Lens.coerced

-- | The retention rule.
schedule_retainRule :: Lens.Lens' Schedule (Prelude.Maybe RetainRule)
schedule_retainRule = Lens.lens (\Schedule' {retainRule} -> retainRule) (\s@Schedule' {} a -> s {retainRule = a} :: Schedule)

-- | The rule for cross-Region snapshot copies.
--
-- You can only specify cross-Region copy rules for policies that create
-- snapshots in a Region. If the policy creates snapshots on an Outpost,
-- then you cannot copy the snapshots to a Region or to an Outpost. If the
-- policy creates snapshots in a Region, then snapshots can be copied to up
-- to three Regions or Outposts.
schedule_crossRegionCopyRules :: Lens.Lens' Schedule (Prelude.Maybe [CrossRegionCopyRule])
schedule_crossRegionCopyRules = Lens.lens (\Schedule' {crossRegionCopyRules} -> crossRegionCopyRules) (\s@Schedule' {} a -> s {crossRegionCopyRules = a} :: Schedule) Prelude.. Lens.mapping Lens.coerced

-- | The rule for enabling fast snapshot restore.
schedule_fastRestoreRule :: Lens.Lens' Schedule (Prelude.Maybe FastRestoreRule)
schedule_fastRestoreRule = Lens.lens (\Schedule' {fastRestoreRule} -> fastRestoreRule) (\s@Schedule' {} a -> s {fastRestoreRule = a} :: Schedule)

instance Core.FromJSON Schedule where
  parseJSON =
    Core.withObject
      "Schedule"
      ( \x ->
          Schedule'
            Prelude.<$> (x Core..:? "VariableTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CreateRule")
            Prelude.<*> (x Core..:? "DeprecateRule")
            Prelude.<*> (x Core..:? "CopyTags")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ShareRules" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "TagsToAdd" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RetainRule")
            Prelude.<*> ( x Core..:? "CrossRegionCopyRules"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "FastRestoreRule")
      )

instance Prelude.Hashable Schedule where
  hashWithSalt salt' Schedule' {..} =
    salt' `Prelude.hashWithSalt` fastRestoreRule
      `Prelude.hashWithSalt` crossRegionCopyRules
      `Prelude.hashWithSalt` retainRule
      `Prelude.hashWithSalt` tagsToAdd
      `Prelude.hashWithSalt` shareRules
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` deprecateRule
      `Prelude.hashWithSalt` createRule
      `Prelude.hashWithSalt` variableTags

instance Prelude.NFData Schedule where
  rnf Schedule' {..} =
    Prelude.rnf variableTags
      `Prelude.seq` Prelude.rnf fastRestoreRule
      `Prelude.seq` Prelude.rnf crossRegionCopyRules
      `Prelude.seq` Prelude.rnf retainRule
      `Prelude.seq` Prelude.rnf tagsToAdd
      `Prelude.seq` Prelude.rnf shareRules
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf deprecateRule
      `Prelude.seq` Prelude.rnf createRule

instance Core.ToJSON Schedule where
  toJSON Schedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VariableTags" Core..=) Prelude.<$> variableTags,
            ("CreateRule" Core..=) Prelude.<$> createRule,
            ("DeprecateRule" Core..=) Prelude.<$> deprecateRule,
            ("CopyTags" Core..=) Prelude.<$> copyTags,
            ("Name" Core..=) Prelude.<$> name,
            ("ShareRules" Core..=) Prelude.<$> shareRules,
            ("TagsToAdd" Core..=) Prelude.<$> tagsToAdd,
            ("RetainRule" Core..=) Prelude.<$> retainRule,
            ("CrossRegionCopyRules" Core..=)
              Prelude.<$> crossRegionCopyRules,
            ("FastRestoreRule" Core..=)
              Prelude.<$> fastRestoreRule
          ]
      )
