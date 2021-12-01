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
-- Module      : Amazonka.Organizations.Types.PolicySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.PolicySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Organizations.Types.PolicyType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a policy, but does not include the content.
-- To see the content of a policy, see DescribePolicy.
--
-- /See:/ 'newPolicySummary' smart constructor.
data PolicySummary = PolicySummary'
  { -- | The Amazon Resource Name (ARN) of the policy.
    --
    -- For more information about ARNs in Organizations, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
    -- in the /AWS Service Authorization Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the policy.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter is a string of any of the characters in the
    -- ASCII character range.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier (ID) of the policy.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
    -- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
    -- letters, digits, or the underscore character (_).
    id :: Prelude.Maybe Prelude.Text,
    -- | A boolean value that indicates whether the specified policy is an AWS
    -- managed policy. If true, then you can attach the policy to roots, OUs,
    -- or accounts, but you cannot edit it.
    awsManaged :: Prelude.Maybe Prelude.Bool,
    -- | The type of policy.
    type' :: Prelude.Maybe PolicyType,
    -- | The description of the policy.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'policySummary_arn' - The Amazon Resource Name (ARN) of the policy.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /AWS Service Authorization Reference/.
--
-- 'name', 'policySummary_name' - The friendly name of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
--
-- 'id', 'policySummary_id' - The unique identifier (ID) of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
--
-- 'awsManaged', 'policySummary_awsManaged' - A boolean value that indicates whether the specified policy is an AWS
-- managed policy. If true, then you can attach the policy to roots, OUs,
-- or accounts, but you cannot edit it.
--
-- 'type'', 'policySummary_type' - The type of policy.
--
-- 'description', 'policySummary_description' - The description of the policy.
newPolicySummary ::
  PolicySummary
newPolicySummary =
  PolicySummary'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      awsManaged = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the policy.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /AWS Service Authorization Reference/.
policySummary_arn :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Text)
policySummary_arn = Lens.lens (\PolicySummary' {arn} -> arn) (\s@PolicySummary' {} a -> s {arn = a} :: PolicySummary)

-- | The friendly name of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
policySummary_name :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Text)
policySummary_name = Lens.lens (\PolicySummary' {name} -> name) (\s@PolicySummary' {} a -> s {name = a} :: PolicySummary)

-- | The unique identifier (ID) of the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
policySummary_id :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Text)
policySummary_id = Lens.lens (\PolicySummary' {id} -> id) (\s@PolicySummary' {} a -> s {id = a} :: PolicySummary)

-- | A boolean value that indicates whether the specified policy is an AWS
-- managed policy. If true, then you can attach the policy to roots, OUs,
-- or accounts, but you cannot edit it.
policySummary_awsManaged :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Bool)
policySummary_awsManaged = Lens.lens (\PolicySummary' {awsManaged} -> awsManaged) (\s@PolicySummary' {} a -> s {awsManaged = a} :: PolicySummary)

-- | The type of policy.
policySummary_type :: Lens.Lens' PolicySummary (Prelude.Maybe PolicyType)
policySummary_type = Lens.lens (\PolicySummary' {type'} -> type') (\s@PolicySummary' {} a -> s {type' = a} :: PolicySummary)

-- | The description of the policy.
policySummary_description :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Text)
policySummary_description = Lens.lens (\PolicySummary' {description} -> description) (\s@PolicySummary' {} a -> s {description = a} :: PolicySummary)

instance Core.FromJSON PolicySummary where
  parseJSON =
    Core.withObject
      "PolicySummary"
      ( \x ->
          PolicySummary'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "AwsManaged")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable PolicySummary where
  hashWithSalt salt' PolicySummary' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` awsManaged
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData PolicySummary where
  rnf PolicySummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf awsManaged
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
