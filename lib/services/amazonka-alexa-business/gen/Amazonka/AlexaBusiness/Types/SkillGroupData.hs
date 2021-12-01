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
-- Module      : Amazonka.AlexaBusiness.Types.SkillGroupData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.SkillGroupData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The attributes of a skill group.
--
-- /See:/ 'newSkillGroupData' smart constructor.
data SkillGroupData = SkillGroupData'
  { -- | The skill group ARN of a skill group.
    skillGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The description of a skill group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The skill group name of a skill group.
    skillGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SkillGroupData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skillGroupArn', 'skillGroupData_skillGroupArn' - The skill group ARN of a skill group.
--
-- 'description', 'skillGroupData_description' - The description of a skill group.
--
-- 'skillGroupName', 'skillGroupData_skillGroupName' - The skill group name of a skill group.
newSkillGroupData ::
  SkillGroupData
newSkillGroupData =
  SkillGroupData'
    { skillGroupArn = Prelude.Nothing,
      description = Prelude.Nothing,
      skillGroupName = Prelude.Nothing
    }

-- | The skill group ARN of a skill group.
skillGroupData_skillGroupArn :: Lens.Lens' SkillGroupData (Prelude.Maybe Prelude.Text)
skillGroupData_skillGroupArn = Lens.lens (\SkillGroupData' {skillGroupArn} -> skillGroupArn) (\s@SkillGroupData' {} a -> s {skillGroupArn = a} :: SkillGroupData)

-- | The description of a skill group.
skillGroupData_description :: Lens.Lens' SkillGroupData (Prelude.Maybe Prelude.Text)
skillGroupData_description = Lens.lens (\SkillGroupData' {description} -> description) (\s@SkillGroupData' {} a -> s {description = a} :: SkillGroupData)

-- | The skill group name of a skill group.
skillGroupData_skillGroupName :: Lens.Lens' SkillGroupData (Prelude.Maybe Prelude.Text)
skillGroupData_skillGroupName = Lens.lens (\SkillGroupData' {skillGroupName} -> skillGroupName) (\s@SkillGroupData' {} a -> s {skillGroupName = a} :: SkillGroupData)

instance Core.FromJSON SkillGroupData where
  parseJSON =
    Core.withObject
      "SkillGroupData"
      ( \x ->
          SkillGroupData'
            Prelude.<$> (x Core..:? "SkillGroupArn")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "SkillGroupName")
      )

instance Prelude.Hashable SkillGroupData where
  hashWithSalt salt' SkillGroupData' {..} =
    salt' `Prelude.hashWithSalt` skillGroupName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` skillGroupArn

instance Prelude.NFData SkillGroupData where
  rnf SkillGroupData' {..} =
    Prelude.rnf skillGroupArn
      `Prelude.seq` Prelude.rnf skillGroupName
      `Prelude.seq` Prelude.rnf description
