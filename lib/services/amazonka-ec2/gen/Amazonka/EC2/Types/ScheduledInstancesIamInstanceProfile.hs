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
-- Module      : Amazonka.EC2.Types.ScheduledInstancesIamInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ScheduledInstancesIamInstanceProfile where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an IAM instance profile for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstancesIamInstanceProfile' smart constructor.
data ScheduledInstancesIamInstanceProfile = ScheduledInstancesIamInstanceProfile'
  { -- | The Amazon Resource Name (ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstancesIamInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'scheduledInstancesIamInstanceProfile_arn' - The Amazon Resource Name (ARN).
--
-- 'name', 'scheduledInstancesIamInstanceProfile_name' - The name.
newScheduledInstancesIamInstanceProfile ::
  ScheduledInstancesIamInstanceProfile
newScheduledInstancesIamInstanceProfile =
  ScheduledInstancesIamInstanceProfile'
    { arn =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN).
scheduledInstancesIamInstanceProfile_arn :: Lens.Lens' ScheduledInstancesIamInstanceProfile (Prelude.Maybe Prelude.Text)
scheduledInstancesIamInstanceProfile_arn = Lens.lens (\ScheduledInstancesIamInstanceProfile' {arn} -> arn) (\s@ScheduledInstancesIamInstanceProfile' {} a -> s {arn = a} :: ScheduledInstancesIamInstanceProfile)

-- | The name.
scheduledInstancesIamInstanceProfile_name :: Lens.Lens' ScheduledInstancesIamInstanceProfile (Prelude.Maybe Prelude.Text)
scheduledInstancesIamInstanceProfile_name = Lens.lens (\ScheduledInstancesIamInstanceProfile' {name} -> name) (\s@ScheduledInstancesIamInstanceProfile' {} a -> s {name = a} :: ScheduledInstancesIamInstanceProfile)

instance
  Prelude.Hashable
    ScheduledInstancesIamInstanceProfile
  where
  hashWithSalt
    salt'
    ScheduledInstancesIamInstanceProfile' {..} =
      salt' `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` arn

instance
  Prelude.NFData
    ScheduledInstancesIamInstanceProfile
  where
  rnf ScheduledInstancesIamInstanceProfile' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf name

instance
  Core.ToQuery
    ScheduledInstancesIamInstanceProfile
  where
  toQuery ScheduledInstancesIamInstanceProfile' {..} =
    Prelude.mconcat
      ["Arn" Core.=: arn, "Name" Core.=: name]
