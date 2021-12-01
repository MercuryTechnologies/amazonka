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
-- Module      : Amazonka.Route53RecoveryControlConfig.DescribeSafetyRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the safety rules (that is, the assertion rules and gating
-- rules) for the routing controls in a control panel.
module Amazonka.Route53RecoveryControlConfig.DescribeSafetyRule
  ( -- * Creating a Request
    DescribeSafetyRule (..),
    newDescribeSafetyRule,

    -- * Request Lenses
    describeSafetyRule_safetyRuleArn,

    -- * Destructuring the Response
    DescribeSafetyRuleResponse (..),
    newDescribeSafetyRuleResponse,

    -- * Response Lenses
    describeSafetyRuleResponse_assertionRule,
    describeSafetyRuleResponse_gatingRule,
    describeSafetyRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | /See:/ 'newDescribeSafetyRule' smart constructor.
data DescribeSafetyRule = DescribeSafetyRule'
  { -- | The request body that you include when you update a safety rule.
    safetyRuleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSafetyRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'safetyRuleArn', 'describeSafetyRule_safetyRuleArn' - The request body that you include when you update a safety rule.
newDescribeSafetyRule ::
  -- | 'safetyRuleArn'
  Prelude.Text ->
  DescribeSafetyRule
newDescribeSafetyRule pSafetyRuleArn_ =
  DescribeSafetyRule'
    { safetyRuleArn =
        pSafetyRuleArn_
    }

-- | The request body that you include when you update a safety rule.
describeSafetyRule_safetyRuleArn :: Lens.Lens' DescribeSafetyRule Prelude.Text
describeSafetyRule_safetyRuleArn = Lens.lens (\DescribeSafetyRule' {safetyRuleArn} -> safetyRuleArn) (\s@DescribeSafetyRule' {} a -> s {safetyRuleArn = a} :: DescribeSafetyRule)

instance Core.AWSRequest DescribeSafetyRule where
  type
    AWSResponse DescribeSafetyRule =
      DescribeSafetyRuleResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSafetyRuleResponse'
            Prelude.<$> (x Core..?> "AssertionRule")
            Prelude.<*> (x Core..?> "GatingRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSafetyRule where
  hashWithSalt salt' DescribeSafetyRule' {..} =
    salt' `Prelude.hashWithSalt` safetyRuleArn

instance Prelude.NFData DescribeSafetyRule where
  rnf DescribeSafetyRule' {..} =
    Prelude.rnf safetyRuleArn

instance Core.ToHeaders DescribeSafetyRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeSafetyRule where
  toPath DescribeSafetyRule' {..} =
    Prelude.mconcat
      ["/safetyrule/", Core.toBS safetyRuleArn]

instance Core.ToQuery DescribeSafetyRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSafetyRuleResponse' smart constructor.
data DescribeSafetyRuleResponse = DescribeSafetyRuleResponse'
  { assertionRule :: Prelude.Maybe AssertionRule,
    gatingRule :: Prelude.Maybe GatingRule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSafetyRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assertionRule', 'describeSafetyRuleResponse_assertionRule' - Undocumented member.
--
-- 'gatingRule', 'describeSafetyRuleResponse_gatingRule' - Undocumented member.
--
-- 'httpStatus', 'describeSafetyRuleResponse_httpStatus' - The response's http status code.
newDescribeSafetyRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSafetyRuleResponse
newDescribeSafetyRuleResponse pHttpStatus_ =
  DescribeSafetyRuleResponse'
    { assertionRule =
        Prelude.Nothing,
      gatingRule = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeSafetyRuleResponse_assertionRule :: Lens.Lens' DescribeSafetyRuleResponse (Prelude.Maybe AssertionRule)
describeSafetyRuleResponse_assertionRule = Lens.lens (\DescribeSafetyRuleResponse' {assertionRule} -> assertionRule) (\s@DescribeSafetyRuleResponse' {} a -> s {assertionRule = a} :: DescribeSafetyRuleResponse)

-- | Undocumented member.
describeSafetyRuleResponse_gatingRule :: Lens.Lens' DescribeSafetyRuleResponse (Prelude.Maybe GatingRule)
describeSafetyRuleResponse_gatingRule = Lens.lens (\DescribeSafetyRuleResponse' {gatingRule} -> gatingRule) (\s@DescribeSafetyRuleResponse' {} a -> s {gatingRule = a} :: DescribeSafetyRuleResponse)

-- | The response's http status code.
describeSafetyRuleResponse_httpStatus :: Lens.Lens' DescribeSafetyRuleResponse Prelude.Int
describeSafetyRuleResponse_httpStatus = Lens.lens (\DescribeSafetyRuleResponse' {httpStatus} -> httpStatus) (\s@DescribeSafetyRuleResponse' {} a -> s {httpStatus = a} :: DescribeSafetyRuleResponse)

instance Prelude.NFData DescribeSafetyRuleResponse where
  rnf DescribeSafetyRuleResponse' {..} =
    Prelude.rnf assertionRule
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf gatingRule
