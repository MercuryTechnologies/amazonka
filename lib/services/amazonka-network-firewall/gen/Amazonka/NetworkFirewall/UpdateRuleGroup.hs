{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.NetworkFirewall.UpdateRuleGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the rule settings for the specified rule group. You use a rule
-- group by reference in one or more firewall policies. When you modify a
-- rule group, you modify all firewall policies that use the rule group.
--
-- To update a rule group, first call DescribeRuleGroup to retrieve the
-- current RuleGroup object, update the object as needed, and then provide
-- the updated object to this call.
module Amazonka.NetworkFirewall.UpdateRuleGroup
  ( -- * Creating a Request
    UpdateRuleGroup (..),
    newUpdateRuleGroup,

    -- * Request Lenses
    updateRuleGroup_ruleGroupArn,
    updateRuleGroup_rules,
    updateRuleGroup_type,
    updateRuleGroup_description,
    updateRuleGroup_ruleGroupName,
    updateRuleGroup_dryRun,
    updateRuleGroup_ruleGroup,
    updateRuleGroup_updateToken,

    -- * Destructuring the Response
    UpdateRuleGroupResponse (..),
    newUpdateRuleGroupResponse,

    -- * Response Lenses
    updateRuleGroupResponse_httpStatus,
    updateRuleGroupResponse_updateToken,
    updateRuleGroupResponse_ruleGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.NetworkFirewall.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRuleGroup' smart constructor.
data UpdateRuleGroup = UpdateRuleGroup'
  { -- | The Amazon Resource Name (ARN) of the rule group.
    --
    -- You must specify the ARN or the name, and you can specify both.
    ruleGroupArn :: Prelude.Maybe Prelude.Text,
    -- | A string containing stateful rule group rules specifications in Suricata
    -- flat format, with one rule per line. Use this to import your existing
    -- Suricata compatible rule groups.
    --
    -- You must provide either this rules setting or a populated @RuleGroup@
    -- setting, but not both.
    --
    -- You can provide your rule group specification in Suricata flat format
    -- through this setting when you create or update your rule group. The call
    -- response returns a RuleGroup object that Network Firewall has populated
    -- from your string.
    rules :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the rule group is stateless or stateful. If the rule
    -- group is stateless, it contains stateless rules. If it is stateful, it
    -- contains stateful rules.
    --
    -- This setting is required for requests that do not include the
    -- @RuleGroupARN@.
    type' :: Prelude.Maybe RuleGroupType,
    -- | A description of the rule group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the rule group. You can\'t change the name of a
    -- rule group after you create it.
    --
    -- You must specify the ARN or the name, and you can specify both.
    ruleGroupName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether you want Network Firewall to just check the validity
    -- of the request, rather than run the request.
    --
    -- If set to @TRUE@, Network Firewall checks whether the request can run
    -- successfully, but doesn\'t actually make the requested changes. The call
    -- returns the value that the request would return if you ran it with dry
    -- run set to @FALSE@, but doesn\'t make additions or changes to your
    -- resources. This option allows you to make sure that you have the
    -- required permissions to run the request and that your request parameters
    -- are valid.
    --
    -- If set to @FALSE@, Network Firewall makes the requested changes to your
    -- resources.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | An object that defines the rule group rules.
    --
    -- You must provide either this rule group setting or a @Rules@ setting,
    -- but not both.
    ruleGroup :: Prelude.Maybe RuleGroup,
    -- | A token used for optimistic locking. Network Firewall returns a token to
    -- your requests that access the rule group. The token marks the state of
    -- the rule group resource at the time of the request.
    --
    -- To make changes to the rule group, you provide the token in your
    -- request. Network Firewall uses the token to ensure that the rule group
    -- hasn\'t changed since you last retrieved it. If it has changed, the
    -- operation fails with an @InvalidTokenException@. If this happens,
    -- retrieve the rule group again to get a current copy of it with a current
    -- token. Reapply your changes as needed, then try the operation again
    -- using the new token.
    updateToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupArn', 'updateRuleGroup_ruleGroupArn' - The Amazon Resource Name (ARN) of the rule group.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'rules', 'updateRuleGroup_rules' - A string containing stateful rule group rules specifications in Suricata
-- flat format, with one rule per line. Use this to import your existing
-- Suricata compatible rule groups.
--
-- You must provide either this rules setting or a populated @RuleGroup@
-- setting, but not both.
--
-- You can provide your rule group specification in Suricata flat format
-- through this setting when you create or update your rule group. The call
-- response returns a RuleGroup object that Network Firewall has populated
-- from your string.
--
-- 'type'', 'updateRuleGroup_type' - Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- This setting is required for requests that do not include the
-- @RuleGroupARN@.
--
-- 'description', 'updateRuleGroup_description' - A description of the rule group.
--
-- 'ruleGroupName', 'updateRuleGroup_ruleGroupName' - The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
--
-- 'dryRun', 'updateRuleGroup_dryRun' - Indicates whether you want Network Firewall to just check the validity
-- of the request, rather than run the request.
--
-- If set to @TRUE@, Network Firewall checks whether the request can run
-- successfully, but doesn\'t actually make the requested changes. The call
-- returns the value that the request would return if you ran it with dry
-- run set to @FALSE@, but doesn\'t make additions or changes to your
-- resources. This option allows you to make sure that you have the
-- required permissions to run the request and that your request parameters
-- are valid.
--
-- If set to @FALSE@, Network Firewall makes the requested changes to your
-- resources.
--
-- 'ruleGroup', 'updateRuleGroup_ruleGroup' - An object that defines the rule group rules.
--
-- You must provide either this rule group setting or a @Rules@ setting,
-- but not both.
--
-- 'updateToken', 'updateRuleGroup_updateToken' - A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the rule group. The token marks the state of
-- the rule group resource at the time of the request.
--
-- To make changes to the rule group, you provide the token in your
-- request. Network Firewall uses the token to ensure that the rule group
-- hasn\'t changed since you last retrieved it. If it has changed, the
-- operation fails with an @InvalidTokenException@. If this happens,
-- retrieve the rule group again to get a current copy of it with a current
-- token. Reapply your changes as needed, then try the operation again
-- using the new token.
newUpdateRuleGroup ::
  -- | 'updateToken'
  Prelude.Text ->
  UpdateRuleGroup
newUpdateRuleGroup pUpdateToken_ =
  UpdateRuleGroup'
    { ruleGroupArn = Prelude.Nothing,
      rules = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing,
      ruleGroupName = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      ruleGroup = Prelude.Nothing,
      updateToken = pUpdateToken_
    }

-- | The Amazon Resource Name (ARN) of the rule group.
--
-- You must specify the ARN or the name, and you can specify both.
updateRuleGroup_ruleGroupArn :: Lens.Lens' UpdateRuleGroup (Prelude.Maybe Prelude.Text)
updateRuleGroup_ruleGroupArn = Lens.lens (\UpdateRuleGroup' {ruleGroupArn} -> ruleGroupArn) (\s@UpdateRuleGroup' {} a -> s {ruleGroupArn = a} :: UpdateRuleGroup)

-- | A string containing stateful rule group rules specifications in Suricata
-- flat format, with one rule per line. Use this to import your existing
-- Suricata compatible rule groups.
--
-- You must provide either this rules setting or a populated @RuleGroup@
-- setting, but not both.
--
-- You can provide your rule group specification in Suricata flat format
-- through this setting when you create or update your rule group. The call
-- response returns a RuleGroup object that Network Firewall has populated
-- from your string.
updateRuleGroup_rules :: Lens.Lens' UpdateRuleGroup (Prelude.Maybe Prelude.Text)
updateRuleGroup_rules = Lens.lens (\UpdateRuleGroup' {rules} -> rules) (\s@UpdateRuleGroup' {} a -> s {rules = a} :: UpdateRuleGroup)

-- | Indicates whether the rule group is stateless or stateful. If the rule
-- group is stateless, it contains stateless rules. If it is stateful, it
-- contains stateful rules.
--
-- This setting is required for requests that do not include the
-- @RuleGroupARN@.
updateRuleGroup_type :: Lens.Lens' UpdateRuleGroup (Prelude.Maybe RuleGroupType)
updateRuleGroup_type = Lens.lens (\UpdateRuleGroup' {type'} -> type') (\s@UpdateRuleGroup' {} a -> s {type' = a} :: UpdateRuleGroup)

-- | A description of the rule group.
updateRuleGroup_description :: Lens.Lens' UpdateRuleGroup (Prelude.Maybe Prelude.Text)
updateRuleGroup_description = Lens.lens (\UpdateRuleGroup' {description} -> description) (\s@UpdateRuleGroup' {} a -> s {description = a} :: UpdateRuleGroup)

-- | The descriptive name of the rule group. You can\'t change the name of a
-- rule group after you create it.
--
-- You must specify the ARN or the name, and you can specify both.
updateRuleGroup_ruleGroupName :: Lens.Lens' UpdateRuleGroup (Prelude.Maybe Prelude.Text)
updateRuleGroup_ruleGroupName = Lens.lens (\UpdateRuleGroup' {ruleGroupName} -> ruleGroupName) (\s@UpdateRuleGroup' {} a -> s {ruleGroupName = a} :: UpdateRuleGroup)

-- | Indicates whether you want Network Firewall to just check the validity
-- of the request, rather than run the request.
--
-- If set to @TRUE@, Network Firewall checks whether the request can run
-- successfully, but doesn\'t actually make the requested changes. The call
-- returns the value that the request would return if you ran it with dry
-- run set to @FALSE@, but doesn\'t make additions or changes to your
-- resources. This option allows you to make sure that you have the
-- required permissions to run the request and that your request parameters
-- are valid.
--
-- If set to @FALSE@, Network Firewall makes the requested changes to your
-- resources.
updateRuleGroup_dryRun :: Lens.Lens' UpdateRuleGroup (Prelude.Maybe Prelude.Bool)
updateRuleGroup_dryRun = Lens.lens (\UpdateRuleGroup' {dryRun} -> dryRun) (\s@UpdateRuleGroup' {} a -> s {dryRun = a} :: UpdateRuleGroup)

-- | An object that defines the rule group rules.
--
-- You must provide either this rule group setting or a @Rules@ setting,
-- but not both.
updateRuleGroup_ruleGroup :: Lens.Lens' UpdateRuleGroup (Prelude.Maybe RuleGroup)
updateRuleGroup_ruleGroup = Lens.lens (\UpdateRuleGroup' {ruleGroup} -> ruleGroup) (\s@UpdateRuleGroup' {} a -> s {ruleGroup = a} :: UpdateRuleGroup)

-- | A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the rule group. The token marks the state of
-- the rule group resource at the time of the request.
--
-- To make changes to the rule group, you provide the token in your
-- request. Network Firewall uses the token to ensure that the rule group
-- hasn\'t changed since you last retrieved it. If it has changed, the
-- operation fails with an @InvalidTokenException@. If this happens,
-- retrieve the rule group again to get a current copy of it with a current
-- token. Reapply your changes as needed, then try the operation again
-- using the new token.
updateRuleGroup_updateToken :: Lens.Lens' UpdateRuleGroup Prelude.Text
updateRuleGroup_updateToken = Lens.lens (\UpdateRuleGroup' {updateToken} -> updateToken) (\s@UpdateRuleGroup' {} a -> s {updateToken = a} :: UpdateRuleGroup)

instance Core.AWSRequest UpdateRuleGroup where
  type
    AWSResponse UpdateRuleGroup =
      UpdateRuleGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRuleGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "UpdateToken")
            Prelude.<*> (x Core..:> "RuleGroupResponse")
      )

instance Prelude.Hashable UpdateRuleGroup where
  hashWithSalt salt' UpdateRuleGroup' {..} =
    salt' `Prelude.hashWithSalt` updateToken
      `Prelude.hashWithSalt` ruleGroup
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` ruleGroupName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` ruleGroupArn

instance Prelude.NFData UpdateRuleGroup where
  rnf UpdateRuleGroup' {..} =
    Prelude.rnf ruleGroupArn
      `Prelude.seq` Prelude.rnf updateToken
      `Prelude.seq` Prelude.rnf ruleGroup
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf ruleGroupName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf rules

instance Core.ToHeaders UpdateRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "NetworkFirewall_20201112.UpdateRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRuleGroup where
  toJSON UpdateRuleGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RuleGroupArn" Core..=) Prelude.<$> ruleGroupArn,
            ("Rules" Core..=) Prelude.<$> rules,
            ("Type" Core..=) Prelude.<$> type',
            ("Description" Core..=) Prelude.<$> description,
            ("RuleGroupName" Core..=) Prelude.<$> ruleGroupName,
            ("DryRun" Core..=) Prelude.<$> dryRun,
            ("RuleGroup" Core..=) Prelude.<$> ruleGroup,
            Prelude.Just ("UpdateToken" Core..= updateToken)
          ]
      )

instance Core.ToPath UpdateRuleGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRuleGroupResponse' smart constructor.
data UpdateRuleGroupResponse = UpdateRuleGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A token used for optimistic locking. Network Firewall returns a token to
    -- your requests that access the rule group. The token marks the state of
    -- the rule group resource at the time of the request.
    --
    -- To make changes to the rule group, you provide the token in your
    -- request. Network Firewall uses the token to ensure that the rule group
    -- hasn\'t changed since you last retrieved it. If it has changed, the
    -- operation fails with an @InvalidTokenException@. If this happens,
    -- retrieve the rule group again to get a current copy of it with a current
    -- token. Reapply your changes as needed, then try the operation again
    -- using the new token.
    updateToken :: Prelude.Text,
    -- | The high-level properties of a rule group. This, along with the
    -- RuleGroup, define the rule group. You can retrieve all objects for a
    -- rule group by calling DescribeRuleGroup.
    ruleGroupResponse :: RuleGroupResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRuleGroupResponse_httpStatus' - The response's http status code.
--
-- 'updateToken', 'updateRuleGroupResponse_updateToken' - A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the rule group. The token marks the state of
-- the rule group resource at the time of the request.
--
-- To make changes to the rule group, you provide the token in your
-- request. Network Firewall uses the token to ensure that the rule group
-- hasn\'t changed since you last retrieved it. If it has changed, the
-- operation fails with an @InvalidTokenException@. If this happens,
-- retrieve the rule group again to get a current copy of it with a current
-- token. Reapply your changes as needed, then try the operation again
-- using the new token.
--
-- 'ruleGroupResponse', 'updateRuleGroupResponse_ruleGroupResponse' - The high-level properties of a rule group. This, along with the
-- RuleGroup, define the rule group. You can retrieve all objects for a
-- rule group by calling DescribeRuleGroup.
newUpdateRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateToken'
  Prelude.Text ->
  -- | 'ruleGroupResponse'
  RuleGroupResponse ->
  UpdateRuleGroupResponse
newUpdateRuleGroupResponse
  pHttpStatus_
  pUpdateToken_
  pRuleGroupResponse_ =
    UpdateRuleGroupResponse'
      { httpStatus = pHttpStatus_,
        updateToken = pUpdateToken_,
        ruleGroupResponse = pRuleGroupResponse_
      }

-- | The response's http status code.
updateRuleGroupResponse_httpStatus :: Lens.Lens' UpdateRuleGroupResponse Prelude.Int
updateRuleGroupResponse_httpStatus = Lens.lens (\UpdateRuleGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateRuleGroupResponse' {} a -> s {httpStatus = a} :: UpdateRuleGroupResponse)

-- | A token used for optimistic locking. Network Firewall returns a token to
-- your requests that access the rule group. The token marks the state of
-- the rule group resource at the time of the request.
--
-- To make changes to the rule group, you provide the token in your
-- request. Network Firewall uses the token to ensure that the rule group
-- hasn\'t changed since you last retrieved it. If it has changed, the
-- operation fails with an @InvalidTokenException@. If this happens,
-- retrieve the rule group again to get a current copy of it with a current
-- token. Reapply your changes as needed, then try the operation again
-- using the new token.
updateRuleGroupResponse_updateToken :: Lens.Lens' UpdateRuleGroupResponse Prelude.Text
updateRuleGroupResponse_updateToken = Lens.lens (\UpdateRuleGroupResponse' {updateToken} -> updateToken) (\s@UpdateRuleGroupResponse' {} a -> s {updateToken = a} :: UpdateRuleGroupResponse)

-- | The high-level properties of a rule group. This, along with the
-- RuleGroup, define the rule group. You can retrieve all objects for a
-- rule group by calling DescribeRuleGroup.
updateRuleGroupResponse_ruleGroupResponse :: Lens.Lens' UpdateRuleGroupResponse RuleGroupResponse
updateRuleGroupResponse_ruleGroupResponse = Lens.lens (\UpdateRuleGroupResponse' {ruleGroupResponse} -> ruleGroupResponse) (\s@UpdateRuleGroupResponse' {} a -> s {ruleGroupResponse = a} :: UpdateRuleGroupResponse)

instance Prelude.NFData UpdateRuleGroupResponse where
  rnf UpdateRuleGroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf ruleGroupResponse
      `Prelude.seq` Prelude.rnf updateToken
